# Project 2: Sorted Sequences

**Problem 1 Due: Friday Nov 5 Before 5:00 PM (10 points)**

**Problem 2 & Bonus Due: Friday Nov 12 Before 5:00 PM (20 points + up to 8 bonus points)**

**Total Points = 30**

## Objectives

In this assignment, you will: 
* Implement a key-value store (Map / Dictionary)
* Use sorted sequences to reduce access costs
* Implement a merge operation
* Create a custom implementation of a mutable.Map
* Think about your design and how to test it.

## Useful Resources

Review the lecture notes and provided example code for some insight into the Scala 
syntax.  You will also want to read the Scala references provided below:

* [The Scala API](https://www.scala-lang.org/api/current/index.html)
  * [Scala Collections](https://docs.scala-lang.org/overviews/collections-2.13/introduction.html)
    * [Ordered](https://www.scala-lang.org/api/current/scala/math/Ordered.html)
    * [IndexedSeq](https://www.scala-lang.org/api/current/scala/collection/IndexedSeq.html)
       - This is a Seq with an efficient apply()
    * [BufferedIterator](https://www.scala-lang.org/api/current/scala/collection/BufferedIterator.html)
       - This is an Iterator with support for `head`, a peek operation.
* [ScalaTest Docs](https://www.scalatest.org/)




Relevant textbook sections:
* Sets and Maps - § 6.2, 6.3 p189-197
* Buffers - § 6.4 p198-199

## Late Policy

The policy for late submissions on assignments is as follows.  Your project grade is 
the best grade over all per-submission grades (or a 0 if no submissions are made).  

For part 1, if a submission is made...
* ... prior to the deadline, your submission is assigned a grade of 100% of the points it earns.
* ... up to 24 hours after the deadline, your submission is assigned a grade of 75% of the points it earns.
* ... more than 24 hours after the deadline, but within 48 hours of the deadline, your submission is assigned a grade of 50% of the points it earns.
* ... more than 48 hours after the deadline, it will not be accepted.

For part 2, if a submission is made...
* ... more than 5 days before the deadline and your submission earns full credit, your submission is assigned a grade of 5 bonus points + 100% of the points it earns. 
* ... up to than 5 days before the deadline and your submission earns full credit, your submission is assigned a grade of 1 bonus point per full day + 100% of the points it earns.
* ... within 24 hours of the deadline, your submission is assigned a grade of 100% of the points it earns.
* ... up to 24 hours after the deadline, your submission is assigned a grade of 75% of the points it earns.
* ... more than 24 hours after the deadline, but within 48 hours of the deadline, your submission is assigned a grade of 50% of the points it earns.
* ... more than 48 hours after the deadline, it will not be accepted.

You will have the ability to use three grace days throughout the semester, and at most 
two per assignment (since submissions are not accepted after two days).  Using a grace 
day will negate the 25% penalty per day, but will not allow you to submit more than 
two days late.  Please plan accordingly.  You will not be able to recover a grace day 
if you decide to work late and your score was not sufficiently higher.  **Grace days 
are automatically applied** to the first instances of late submissions, **and are 
non-refundable**.  For example, if an assignment is due on a Friday and you make a 
submission on Saturday, you will automatically use a grace day, regardless of whether 
you perform better or not.  **Be sure to test your code before submitting**, especially 
with late submissions **in order to avoid wasting grace days**.

**Keep track of the time if you are working up until the deadline**.  Submissions 
become late after the set deadline.  Keep in mind that **submissions will close 48
hours after the original deadline** and you will not be able to submit your code after 
that time.



## Project Setup

1. Use your existing project 1 repository **or** clone a fresh copy:
```bash
git clone microbase@odin.cse.buffalo.edu:YOUR_UBIT.git
```
(don't forget to replace `YOUR_UBIT`)

2. Update your repository to include materials for PA2 as follows.  From your 
source directory, run the following commands at the command line.
```bash
git remote add project-2 git@github.com:UBOdin/cse-250-pa-sortedsequences.git
git fetch project-2
git merge project-2/main
```

3. Update the copyright statement with your UBIT and person number in the
   submission files.

4. Review the file contents and read over the comments on what is already 
   present.  

## Instructions

The following assignment consists of two parts.  Before submitting either part
make sure that all of your code is committed and pushed into microbase.  

**Only code in the `src/test/scala/cse250/pa2` directory will be considered for 
part 1, and only code in the `src/main/scala/cse250/pa2` directory will be 
considered for part 2.**

Once your code is committed and pushed into microbase, log into 
[microbase](https://microbase.odin.cse.buffalo.edu) and submit your assignment
to the "Programming Project 2 - Tests" or "Programming Project 2 - 
Implementation" projects for part 1 and part 2 respectively.

**Expect this project to take 8-10 hours of setting up your environment, reading 
through documentation, and planning, coding, and testing your solution.**

For **Project 2 - Problems 1 and 2** you will be allowed an unlimited number of submissions without penalty.  For **Project 2 - Problem 2**, you will receive the first 60% of your grade (up to 12 points) immediately from microbase.  After the late submission deadline, your final score will be computed based on **the most recent submission**.


### Overview

In this project, you will implement classes called `LSMIndex` and `UniqueLSMIndex`, 
which implement a data structure called a "Log-Structured Merge Tree" (LSM Tree, or LSM
Index for short; see the [original paper](papers/lsmtree.pdf) and an [early follow-up](papers/blsmtree.pdf)).  Note that, **in spite of the term "tree" in its name, the LSM Index
is not a tree in the same sense as a heap or binary search tree**.  LSM trees are used 
extensively in big-data processing systems, including: 
* [Google's BigTable](https://cloud.google.com/bigtable/)
* [Apache Cassandra](http://cassandra.apache.org/)
* [LevelDB](https://github.com/google/leveldb)
* [MongoDB](https://github.com/wiredtiger/wiredtiger)

As we'll see in upcoming written assignments, the LSM Index has some very nice properties, 
particularly for "write heavy" workloads like monitoring IOT data, or querying log files in distributed 
clusters.  In these applications, users want to be able to efficiently retrieve records by some identifier (something that a sorted array is great at), while also being able to ingest lots of data very quickly (something that sorted lists, and other organizational data structures like binary search trees or their cache-friendly cousin, B+Trees are very bad at).

An LSM Index stores data in a sequence of exponentially growing levels (sometimes called layers or tiers), where every layer except the 0th is (i) a **sorted** array, and (ii) immutable.  Any given layer may be occupied or not.  There are typically a few additional structural components at each layer (a fence pointer table, a bloom filter), but we will ignore them here.

Concretely: For some "buffer size" $`B`$, an LSM Index will store
* The Buffer: A mutable array of size up to $`B`$
* Level 0: Nothing, or one immutable, sorted array of size $`B`$
* Level 1: Nothing, or one immutable, sorted array of size $`2B`$
* Level 2: Nothing, or one immutable, sorted array of size $`4B`$
* ...
* Level j: Nothing, or one immutable, sorted array of size $`2^j B`$

Insertions always happen into the buffer (see below for a discussion of deletions):

When the buffer fills up (i.e., reaches size $`B`$), the $`B`$ ($`=2^0 B`$) elements in the buffer are sorted and "promoted" to Level 0.  Once the buffered records are promoted, the buffer is cleared.

When an array of $`2^j B`$ elements is promoted to level $`j`$, one of two things happens, depending on whether level $`j`$ is currently occupied:
* If the level is not already occupied (i.e., there is nothing stored at the level), the newly promoted array is inserted at the level processing stops.
* If the level **is** already occupied (i.e., there is an array stored at the level), the newly promoted array is merged with it to create a new sorted array (of size $`2^{j+1} B`$).  The merged array is promoted to level $`j+1`$.  Once the records are promoted, level $`j`$ is no longer occupied.

#### Example
Let's take an LSM Index (with $`B = 100`$) that initially contains 2032 elements:
```
Buffer: 32 elements
Level 0: unoccupied
Level 1: unoccupied
Level 2: [Sorted Immutable Sequence of 400 elements]
Level 3: unoccupied
Level 4: [Sorted Immutable Sequence of 1600 elements]
```

##### Step 1 

After we insert 68 records (32+68 = 100), 
* The buffer is sorted, and promoted to level 0.
```
Buffer: 0 elements
Level 0: [Sorted Immutable Sequence of 100 elements]
Level 1: unoccupied
Level 2: [Sorted Immutable Sequence of 400 elements]
Level 3: unoccupied
Level 4: [Sorted Immutable Sequence of 1600 elements]
```

##### Step 2

Elements continue to be inserted into the buffer.  After another 100 insertions, the buffer fills up:
* The buffer is sorted, and promoted to level 0
* Level 0 is already occupied, so the 100 elements of the sorted buffer are merged with the 100 elements at level 0.  The resulting 200-element sequence is promoted to level 1.
```
Buffer: 0 elements
Level 0: unoccupied
Level 1: [Sorted Immutable Sequence of 200 elements]
Level 2: [Sorted Immutable Sequence of 400 elements]
Level 3: unoccupied
Level 4: [Sorted Immutable Sequence of 1600 elements]
```

##### Step 3

Elements continue to be inserted to the buffer.  After another 100 insertions, the buffer fills up:
* The buffer is sorted, and promoted to level 0.
```
Buffer: 0 elements
Level 0: [Sorted Immutable Sequence of 100 elements]
Level 1: [Sorted Immutable Sequence of 200 elements]
Level 2: [Sorted Immutable Sequence of 400 elements]
Level 3: unoccupied
Level 4: [Sorted Immutable Sequence of 1600 elements]
```

##### Step 4 

Elements continue to be inserted to the buffer.  After another 100 insertions, the buffer fills up:
* The buffer is sorted, and promoted to level 0
* Level 0 is already occupied, so the 100 elements of the sorted buffer are merged with the 100 elements at level 0.  The resulting 200-element sequence is promoted to level 1.
* Level 1 is already occupied, so the 200 elements promoted from level 0 are merged with the 200 elements at level 1.  The resulting 400-element sequence is promoted to level 2.
* Level 2 is already occupied, so the 400 elements promoted from level 1 are merged with the 400 elements at level 2.  The resulting 800-element sequence is promoted to level 3.
```
Buffer: 0 elements
Level 0: unoccupied
Level 1: unoccupied
Level 2: unoccupied
Level 3: [Sorted Immutable Sequence of 800 elements]
Level 4: [Sorted Immutable Sequence of 1600 elements]
```

##### Step 5

After another 799 insertions, the first three levels of the LSM index and buffer would fill up again:
```
Buffer: 99 elements
Level 0: [Sorted Immutable Sequence of 100 elements]
Level 1: [Sorted Immutable Sequence of 200 elements]
Level 2: [Sorted Immutable Sequence of 400 elements]
Level 3: [Sorted Immutable Sequence of 800 elements]
Level 4: [Sorted Immutable Sequence of 1600 elements]
```

##### Step 6

The very next insertion after this would bring the number of records in the LSM tree to 3200, requiring promotions at level 0, 1, 2, 3, and 4.  A new level (5) would need to be created
```
Buffer: 0 elements
Level 0: unoccupied
Level 1: unoccupied
Level 2: unoccupied
Level 3: unoccupied
Level 4: unoccupied
Level 5: [Sorted Immutable Sequence of 3200 elements]
```
 

### Problem 1: Tests
(10 points)

Your first task is to write tests that determine if an implementation of 
`LSMIndex` adheres to the requirements specified below 
for each of the operations.  In particular, you should ensure that each of
the expected behaviors follow from each of the method calls that are to be
implemented.  For example, if you insert a value then subsequent calls to 
apply should receive the value.

You must implement your tests in the `LSMIndexTests` class, located in `src/test/scala/cse250/pa2/`.  From within these
classes, you must call the `lsmIndex` method to obtain
empty instances of the classes under test.  Simple example tests are 
already present.

In order to write good tests, you should look through the specification for 
each method and ensure that anywhere a behavior is specified, you should 
write a test that performs a sequence of method calls necessary to expose a 
problem and to ensure that the expected result has occurred.  This will 
require creating multiple scenarios with various operation sequences.  
Similarly if you have requirements that must always hold, you may want to add 
assertions between method calls that these still hold.

A few notes:

* The tests will not have access to any data files.  You should write your 
  tests without loading any files.
* If you add members to any classes being tested,  you should avoid 
  submitting tests that access these fields as they will not compile.  Your 
  tests should limit access to only what is public in the handout code.
* Your tests should generally focus on testing **specified** behavior.  This 
  means specifically behaviors documented below.  For the most part, specified
  behaviors are limited to the API (i.e., the object's callable methods such
  as apply, iterator, remove, etc...).  If the specification does not 
  explicitly say something about how the data is to be organized, it should 
  not be part of the test.
* The assert method has an optional 2nd parameter that allows you to provide a
  debug message.  If the assertion fails, this message will be included in the
  testing log.

Your code will be tested against correct and incorrect implementations of 
`LSMIndex`.  Your goal is to get all tests to pass on 
the correct implementation, and for at least one test to fail for an incorrect implementation.  

A testing suite that fails a valid implementation will receive 0 points.  A 
testing suite that passes a valid implementation will be graded based on 
the incorrect implementations it fails.

### Problem 2: Append-Only LSM Index
(20 points)

Your task is to implement an LSM Index (`LSMIndex[K, V]`) and a helper class `MergedIterator`.  

The MergedIterator is constructed from two existing iterators that are expected to produce values
in ascending sorted order.  One method remains unimplemented:

#### next: A
Return the smaller of the two values at the head of the left or right iterator and advance the 
corresponding iterator to the next step.

---

The LSM Index is a collection of key-value pairs, where keys have type `K` and values have type 
`V`.  There are three variables pre-defined:

* `_buffer`: A fixed-size `_bufferSize`-element buffer that newly inserted key-value are to be inserted into.  This buffer does not need to be sorted.

* `_bufferElementsUsed`: The number of elements of `_buffer` that are used.

* `_levels`: The collection of levels of the LSM index. A level may be occupied (`Some(elements)`) or empty (`None`).  
    * If `_levels(i)` is defined, it **must** contain an *immutable*, *sorted* list of exactly $`2^i \cdot \texttt{\_bufferSize}`$ elements.  

You will need to implement the following methods:


#### `promote(level: Int, elements: IndexedSeq[(K, V)]): Unit`

If level `level` has not been allocated (`_levels.size <= level`), allocate it as an unoccupied level.

If level `level` is not occupied, place the already sorted sequence `elements` at that level (i.e., after `promote`, the level should be occupied).

If level `level` is occupied, merge `elements` with the current contents of the level and promote the result to the next level.  You may find the `MergedIterator` class defined above helpful.

#### `contains(key: K): Boolean`

Determine if the provided key is present in the LSM index.  Return true if so.

#### `apply(key: K): Seq[V]`

Retrieve every key-value pair with the provided key present in the LSM Index.

* You may find the `search` method of [IndexedSeq](https://www.scala-lang.org/api/current/scala/collection/IndexedSeq.html) helpful.


### Bonus Problem: LSM Index with Unique Keys and Deletion
(3 points)

As a bonus objective, implement an LSM Index (`UniqueLSMIndex[K, V]`) that guarantees that only the most recently inserted value for each key will be returned by `apply`, and that allows deletions.

Single-valued keys can be accomplished without requiring mutable sequences through two observations:

1. More recently inserted elements are always at lower levels (or in the buffer) -- the value in the buffer is always more recent than the value for the same key at level 2, which is more recent than the value for the same key at level 4.

2. A redundant, older value can be eliminated when the level is merged through a promotion.

Deletions can be accomplished by "inserting" a special marker, usually called a tombstone, that indicates that the value for the key was deleted.  The tombstone overwrites older records during merges.  Once it reaches the final level, it can be expired.  In `UniqueLSMIndex`, you'll find that `_level` uses a value type of `Option[V]` --- Use `None` as a tombstone marker.

You will need to change your implementation in at least the following ways:

1. Apply will need to prioritize returning values at lower levels
2. Insert will need to de-duplicate the buffer.
3. When merging layers together, you will need to de-duplicate keys and apply tombstones.


## Suggested approach


1. Read over the tasks and make notes on what each part is supposed to do and
   what invariants it should maintain.

2. Write your tests to the extent that you feel it would be sufficient that if
   you pass all of them your work is complete.

3. Implement the `MergeIterator`.  Note the [BufferedIterator.head](https://www.scala-lang.org/api/current/scala/collection/BufferedIterator.html#head:A) method.  

4. Implement `promote`.  Note the [Iterator.toIndexedSeq](https://www.scala-lang.org/api/current/scala/collection/BufferedIterator.html#toIndexedSeq:IndexedSeq[A]) method.

5. Implement `apply`.  Note the [IndexedSeq.search](https://www.scala-lang.org/api/current/scala/collection/immutable/IndexedSeq.html#search[B%3E:A](elem:B)(implicitord:scala.math.Ordering[B]):collection.Searching.SearchResult) method.

6. Implement `contains`

It is particularly important to follow some semblance of this approach when
working on the assignment, as it will be confusing to work out of this order.
For instance, it doesn't make sense trying to work on removal of data when 
nothing is stored.  

Be sure to test as you go.  **Don't wait until the end to test!**



## Allowed library/container usage

Your code may include any containers from the scala collection library, but note that conformance to
the structure defined above will be tested for.  You may find the following methods useful for 
creating new containers.
* `.iterator`
* `.toIndexedSeq`
* `Some()`
* `None`


## AI Policy Overview

As a gentle reminder, please re-read the academic integrity policy of the course.  I 
will continue to remind you throughout the semester and hope to avoid any incidents.

### What constitutes a violation of academic integrity?

These bullets should be obvious things not to do (but commonly occur):
* Turning in your friend's code/write-up (obvious).
* Turning in solutions you found on Google with all the variable names changed (should be obvious).  This is a copyright violation, in addition to an AI violation.
* Turning in solutions you found on Google with all the variable names changed and 2 lines added (should be obvious).  This is also a copyright violation.
* Use of Github Autopilot (should be obvious).  This is still in murky legal water, and may be a copyright violation, in addition to being an AI violation.
* Paying someone to do your work.  You may as well not submit the work, since you will fail the exams and the course.
* Posting to forums asking someone to solve assignment problems *(even if you do not receive the solution)*
* Accessing solutions to assignment problems.  

**Note:** Aggregating every { stack overflow answer, result from google, other 
source } because you "understand it" will likely result in full credit on assignments
(if you are not caught), and then failure on every exam.  Exams don't test if you know
how to use Google, but rather test your understanding (i.e., do you understand the 
problem and material well enough to arrive at a solution on your own).  Also, other
students are likely doing the same thing, and then you will be wondering why 10 people
that you don't know have your exact solution.

### What collaboration is allowed?

There is a grey area when it comes to discussing the problems with your peers, and I 
do encourage you to work with one another to discuss course *concepts* related to 
an assignment.  That is the best way to learn and to overcome obstacles.  At the same 
time, you need to be sure you do not overstep and not plagiarize.  Discussions pointing
to relevant course materials are OK.  For example, the following is acceptable advice:

> It would be helpful to review the usage of the stack in the recitation slides from
> week XX.

When working with your peers, we ask that you include attribution; In the header 
comment of the Main function of your submission, please list all peers who you have
discussed the project with.

Explaining every step in detail and/or giving pseudocode that solves the problem
is **not ok**.  For example, the following is **not acceptable** advice:

> I copied the algorithm from the week XX notes into my code at the start of the
> function, created a function that went through the given data and put it into a list, 
> called that function, and then sorted the results.

The first example is OK.  The second example, however, is a summary of your code and
is not acceptable.  Remember that you should never show any of your code to other 
students prior to any deadlines.  Regardless of where you are working, you must always
follow this rule: **Never come away from discussions with your peers with any written
work, either typed or photographed, and especially do not share or allow viewing of
your written code**. 

If you have concerns that you may have overstepped or worked too closely with someone,
please address this with me prior to deadlines for the assignment.  **Even if you have
submitted an assignment that may have violated the course academic integrity policy,
if you approach me *prior to detection* you will not face academic integrity 
proceedings**.  We will address options at that point.

### What resources are allowed?

With all of this said, please feel free to use any { files | examples | tutorials } 
that course staff provides, directly in your code.  Feel free to directly use any
materials from lecture or recitations.  You will never be penalized for doing so,
but must always provide attribution/citation for where you retrieved code from. Just
remember, if you are citing an algorithm that is not provided by us, then you are 
probably overstepping.  

More explicitly, you may use any of the following resources (with proper 
citation/annotation in your code: 
* Any example files posted on the course webpage or Piazza (from lecture or recitation)
* Any code that the instructor provides
* Any code that the TAs provide
* Any code from the [Tour of Scala](https://docs.scala-lang.org/tour/tour-of-scala.html)
* Any code from [Scala Collections](https://docs.scala-lang.org/overviews/collections-2.13/introduction.html)
* Any code from [Scala API](https://www.scala-lang.org/api/2.13.0/)
* Additional references may be provided as the semester progresses, but only those provided publicly by course staff are allowed for use.  These will be listed on Piazza under Resources

**Omitting citation/attribution will result in an AI violation** (and lawsuits later 
in life at your job).  This is true, **even if you are using provided resources**.

Again, **if you think you are going to violate/have violated this policy, please come
talk to a member of the course staff ASAP so we can figure out how to get you on track 
to succeed in the course**.  If you have a question about the validity of a resource, 
please ask a TA or your instructor prior to using it.  If you have already used it, 
please discuss with the instructor to determine a workaround and, at the very least,
avoid an academic integrity infraction.  For example, you might send an email such
as the following to the course instructor:

> Clarus T Example<br/>
> **UBIT**: ctexamp<br/>
> **Person** #: 123456789
> 
> Dear Dr. Kennedy,
> 
> I believe that I may have submitted work that is *{ not fully my own | not properly
> attributed }*.  I wish to retract my submission to preserve academic integrity in
> the course.
> 
> Signed,<br/>
> &nbsp;&nbsp;Clarus T Example

This policy on assignments is here so that you learn the material and how to think 
for yourself.  There is no cognitive benefit achieved by submitting solutions someone
else has written (which likely already exist in some form).

## Collaboration Policy

The policy for collaboration on assignments is as follows:
* All work for this course must be original individual work.
* You must follow the limits on collaboration as defined in the AI policy (i.e., no shared code/etc...)
* You must identify any collaborators (first and last name) on every assignment.  THis can be in a comment at the top of your code submissions or on the first page at the top of your written work, beside your name.

All references must be cited using a comment containing a direct link to the resource, 
as well as a brief description of what was used.  For example, if you reference the 
textbook, a page number and description is sufficient.  If you copy example code from 
the Scala Language API, then include the link to the class page within the API as well
as where the example code resides.


## Revision History
* Fall 2021 - Oliver Kennedy (okennedy@buffalo.edu)
