# Project 1: Sequences

**Part 1 Due: Friday 10/15/21 Before 5:00 PM (10 points)**

**Part 2 Due: Friday 10/22/21 Before 5:00 PM (20 points + up to 7 bonus points)**

**Total Points = 30**

## Objectives

In this assignment, you will: 
* Define a custom type
* Create a custom implementation of a mutable.Seq
* Explore the basics of linked records
* Implement a linked list using an Array to represent memory
* Think about your design and how to test it.

## Useful Resources

Review the lecture notes and provided example code for some insight into the Scala 
syntax.  You will also want to read the Scala references provided below:

* [The Scala API](https://www.scala-lang.org/api/current/index.html)
  * [Scala Collections](https://docs.scala-lang.org/overviews/collections-2.13/introduction.html)
* [ScalaTest Docs](https://www.scalatest.org/)

Relevant textbook sections:
* Nature of Arrays / Nature of Lists - § 12.2, 12.3 p406-408
* Mutable Singly Linked List - § 12.4 p408-417
* Mutable Doubly Linked List - § 12.5 p417-422

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

1. Update your repository to include materials for PA1 as follows.  From your 
source directory, run the following commands at the command line.
```bash
git remote add project-1 https://gitlab.odin.cse.buffalo.edu/cse-250/project-1.git
git fetch project-1
git merge project-1/main
```

2. Update the copyright statement with your UBIT and person number in the
   submission files.

3. Review the file contents and read over the comments on what is already 
   present.  

## Instructions

The following assignment consists of two parts.  Before submitting either part
make sure that all of your code is committed and pushed into microbase.  

**Only code in the `src/test/scala/cse250/pa1` directory will be considered for 
part 1, and only code in the `src/main/scala/cse250/pa1` directory will be 
considered for part 2.**

Once your code is committed and pushed into microbase, log into 
[microbase](https://microbase.odin.cse.buffalo.edu) and submit your assignment
to the "Programming Project 1 - Tests" or "Programming Project 1 - 
Implementation" projects for part 1 and part 2 respectively.

**Expect this project to take 8-10 hours of setting up your environment, reading 
through documentation, and planning, coding, and testing your solution.**

For **Project 1** you will be allowed 5 submissions, without penalty.  Starting 
from the 6th submission, you will receive a 2 points per-submission deduction 
from your score on the assignment.

Your score is the best score of any of your submissions.  If you receive a score
and resubmit, the higher score will be used.  There is a maximum of 10 
submissions.

### Overview

In this project, you will implement a class called `LinkedListBuffer` to 
implement a bounded-capacity linked list.  Buffer elements will reside in a
fixed-size `Array` that represents a pre-allocated region of memory.  Array 
indices will be used in place of pointers.

### Problem 1: Tests
(10 points)

Your first task is to write tests that determine if an implementation of 
`LinkedListBuffer` adheres to the requirements specified below for each of the
operations.  In particular, you should ensure that the expected behaviors follow
from each of the method calls that are to be implemented.  For example, if you 
append a value and an iterator over the sequence does not include the value, 
this would not be a correct implementation.

You must implement your tests in the `LinkedListBufferTests` class, located
in `src/test/scala/cse250/pa1/LinkedListBufferTests.scala`.  From within this
class, you may call the `createLinkedListBuffer` method to obtain an empty 
instance of the `LinkedListBuffer` implementation under test.  An example test 
is already present.

In order to write good tests, you should look through the specification for each
method and ensure that anywhere a behavior is specified, you should write a test
that performs a sequence of method calls necessary to expose a problem and to
ensure that the expected result has occurred.  This will require creating 
multiple scenarios with various operation sequences.  Similarly if you have 
requirements that must always hold, you may want to add assertions between 
method calls that these still hold.

A few notes:

* The tests will not have access to the dataset file.  You should write your 
  tests without loading any files.
* If you add members to the `LinkedListBuffer` class, you should avoid 
  submitting tests that access these fields as they will not compile.  Your 
  tests should limit access to only what is public in the handout code.
* Your tests should generally focus on testing **specified** behavior.  This 
  means specifically behaviors documented below.  For the most part, specified
  behaviors are limited to the API (i.e., the object's callable methods such
  as apply, iterator, remove, etc...).  If the specification does not 
  explicitly say something about how the data is to be organized, it should not
  be part of the test.
* The assert method has an optional 2nd parameter that allows you to provide a
  debug message.  If the assertion fails, this message will be included in the
  testing log.

Your code will be tested against correct and incorrect implementations of 
`LinkedListBuffer`.  Your goal is to get all tests to pass on the correct 
implementation, and for at least one test to fail for an incorrect 
implementation.

### Problem 2: Implementation
(20 points)

Your task is to build a mutable sequence that stores a bounded number of 
elements in the order in which they are provided through a series of insertions
and removals.  For this assignment, you will have to implement the specified API
using a specific internal storage scheme.  The internal storage we will be using
is a fixed-size `Array` holding an embedded list of type `A` objects.  Unlike
a typical `Array` however, the position of each object is **not** its position
in the `Array`.  Instead the "nodes" of the list keep note of the indices within
the array to find the nodes that come before and after in the sequence, like a
Linked List.

To realize this sequence, your task is to complete the definition of 
`LinkedListBuffer` by implementing the `mutable.Seq` trait combined with 
additional methods as follows.  Note that these are implemented using a generic
type `A`.  If you look at the provided tests and the main method, you will see
that we can use this with the `SolarInstallation` class.

##### `append(entry: A): Option[A]`
* Record `entry` into your data store.
    * The newest entry mut always be stored at the end (tail) of the sequence
    * Any available slot (empty node) in your backing storage array may be used
      when one is available.  
* Update `_numStored` if necessary.  If `capacity` is reached, `_numStored` 
  should not increase.
* If the capacity of the backing storage array is reached, you should overwrite
  the oldest entry (the head) with `entry`.  
    * This would make the second oldest entry the new oldest (the new head)
    * This would still make `entry` the newest entry (the new tail)
    * If an entry is replaced, you should return the replaced entry, otherwise return `None`.
* Duplicate entries are OK.  We only care about storing them in sequence of 
  oldest to newest (based on insertion order).
* The runtime of this function must be O(capacity) (i.e., linear in the maximum
  size of the sequence) in general.  
    * In the specific case where the capacity of the list has been reached and
      the size of the list is not changed by this method, the function must have
      a runtime of Θ(1) (i.e., constant time when `length` = `capacity`).
    * 2 bonus points will be awarded if the wall-clock performance of this 
      function suggests that it always has a runtime of Θ(1) (i.e., constant 
      time for all calls)

##### `remove(entry: A): Boolean`
* If `entry` is not currently present in the list, return `false`.  Otherwise,
  remove all records containing `entry` and return `true`.  
    * Test for equivalence using `==`.
* Update `_numStored` if necessary.
* Any values that are not equal to `entry` should not be moved within `_buffer`.
* The runtime of this function must be O(length) (i.e., linear in the number of 
  elements currently in the sequence)


##### `countEntry(entry: A): Int`
* Return a count of the nodes containing the given `entry`.
    * Test for equivalence using `==`.
* The runtime of this function must be O(length) (i.e., linear in the number of 
  elements currently in the sequence)

##### `apply(idx: Int): A`
* Return the value of the entry at index `idx` within the sequence (0-based indexing)
* Required by `mutable.Seq`
* The runtime of this function must be O(idx) (i.e., linear in the index being
  retrieved).
* If the index provided is not present (or greater than capacity), the behavior of this function is unspecified.  It is customary to throw an `IndexOutOfBoundsException`.


##### `update(idx: Int, elem: A): Unit`
* Update the entry at index `idx` within the sequence (0-based indexing) to be  `elem`
* Required by `mutable.Seq`
* If the index provided is not present (or greater than capacity), the behavior of this function is unspecified.  It is customary to throw an `IndexOutOfBoundsException`.
* The runtime of this function must be O(idx) (i.e., linear in the index being
  retrieved).

##### `length`
* Return the number of elements currently present in the sequence
* Required by `mutable.Seq`
* The runtime of this function must be Θ(1) (i.e., constant)

##### `iterator`
* Return an iterator over the elements of the list.  Elements must be visited
  in order from oldest to newest.
    * For your convenience, a template iterator class is defined in the same
      file.  `LinkedListIterator` is a member class of `LinkedListBuffer`, and
      as such, its methods have ha access to all instance variables in the 
      enclosing `LinkedListBuffer` that created it.
    * Functional `hasNext()` and `next()` methods have already been implemented.
* Required by `mutable.Seq`
* The iterator's `remove` method must delete the last element that the `next` 
  method returned.  The method must have a runtime of Θ(1) (i.e., constant, 
  per-call)
    * Note that, unlike the `remove` method on `LinkedListBuffer`, this method
      **only ever removes a single element.**
    * Note that you may need to modify other methods in `LinkedListIterator` to
      implement this method.

### Linked List Organization

You must maintain a "linked list" of entries stored in `_buffer` of your 
`LinkedListBuffer`.  The nodes of the list are stored within the array as 
`LinkedListNode` objects.  A `LinkedListNode` contains a `_value` of type 
`Option[A]`, which may be either `None` if the node is not assigned, or `Some`
if the node is in-use.  `_value.get` will return the value of the node if it is
assigned.  You may use `_value.isDefined` to test whether the node has a value.
The `LinkedListNode` also has indices for `prev` and `next` of type `Int`.  

You should keep in mind the following pointers while maintaining your embedded 
linked list:
* When an entry is added, be sure to update the indices of the `_tail`, the 
  modified node, and possibly `_head` as well.
* When an entry is removed, be sure to update the indices of the surrounding
  entries, and possibly `_head` and `_tail` as well.
* Take care to check if you are removing the head and/or tail and handle these
  cases separately.
* **Hint**: If the storage is full, think of insertion as if you were to remove the 
  head of the list and then use that slot to store the new tail.

Consider the following code:

```scala
/* 1.  */ val buffer = new LinkedListBuffer[SolarInstallation](4)
/* 2.  */ val e1, e2, e3, e4, e5 = new SolarInstallation() // allocate 5 objects
/* 3.  */ // Normally you would initialize e1-e5 here.
/* 4.  */ buffer.append(e1)
/* 5.  */ buffer.append(e2)
/* 6.  */ buffer.append(e3)
/* 7.  */ buffer.append(e4)
/* 8.  */ buffer.append(e3)
/* 9.  */ buffer.remove(e3)   // returns true
/* 10. */ buffer.append(e5)
/* 11. */ val iter = buffer.iterator
/* 12. */ iter.next()         // returns e2
/* 13. */ iter.next()         // returns e4
/* 14. */ iter.remove()
```

The following is a visualization of the internal state of `buffer` for the above
code.  

##### Following Line 1 execution

<table>
  <tr>
    <th colspan=2>None</th>
    <th colspan=2>None</th>
    <th colspan=2>None</th>
    <th colspan=2>None</th>
  </tr>
  <tr>
    <td></td>
    <td></td>
    <td></td>
    <td></td>
    <td></td>
    <td></td>
    <td></td>
    <td></td>
  </tr>
</table>

```
capacity: 4
_numStored: 0
_head: -1
_tail: -1
```
This represents an empty list, internally.

##### Following Line 4 execution

<table>
  <tr>
    <th colspan=2>Some(e1)</th>
    <th colspan=2>None</th>
    <th colspan=2>None</th>
    <th colspan=2>None</th>
  </tr>
  <tr>
    <td>-1</td>
    <td>-1</td>
    <td></td>
    <td></td>
    <td></td>
    <td></td>
    <td></td>
    <td></td>
  </tr>
</table>

```
capacity: 4
_numStored: 1
_head: 0
_tail: 0
```
This corresponds to the list `Seq( e1 )`

##### Following Line 5 execution

<table>
  <tr>
    <th colspan=2>Some(e1)</th>
    <th colspan=2>Some(e2)</th>
    <th colspan=2>None</th>
    <th colspan=2>None</th>
  </tr>
  <tr>
    <td>-1</td>
    <td>1</td>
    <td>0</td>
    <td>-1</td>
    <td></td>
    <td></td>
    <td></td>
    <td></td>
  </tr>
</table>

```
capacity: 4
_numStored: 2
_head: 0
_tail: 1
```
This corresponds to the list `Seq( e1, e2 )`, stored as `[e1]` ↔ `[e2]`

##### Following Line 6 execution

<table>
  <tr>
    <th colspan=2>Some(e1)</th>
    <th colspan=2>Some(e2)</th>
    <th colspan=2>Some(e3)</th>
    <th colspan=2>None</th>
  </tr>
  <tr>
    <td>-1</td>
    <td>1</td>
    <td>0</td>
    <td>2</td>
    <td>1</td>
    <td>-1</td>
    <td></td>
    <td></td>
  </tr>
</table>

```
capacity: 4
_numStored: 3
_head: 0
_tail: 2
```
This corresponds to the list `Seq( e1, e2, e3 )`, 
stored as `[e1]` ↔ `[e2]` ↔ `[e3]`

##### Following Line 7 execution

<table>
  <tr>
    <th colspan=2>Some(e1)</th>
    <th colspan=2>Some(e2)</th>
    <th colspan=2>Some(e3)</th>
    <th colspan=2>Some(e4)</th>
  </tr>
  <tr>
    <td>-1</td>
    <td>1</td>
    <td>0</td>
    <td>2</td>
    <td>1</td>
    <td>3</td>
    <td>2</td>
    <td>-1</td>
  </tr>
</table>

```
capacity: 4
_numStored: 4
_head: 0
_tail: 3
```
This corresponds to the list `Seq( e1, e2, e3, e4 )`, 
stored as `[e1]` ↔ `[e2]` ↔ `[e3]` ↔ `[e4]`

##### Following Line 8 execution

<table>
  <tr>
    <th colspan=2>Some(e3)</th>
    <th colspan=2>Some(e2)</th>
    <th colspan=2>Some(e3)</th>
    <th colspan=2>Some(e4)</th>
  </tr>
  <tr>
    <td>3</td>
    <td>-1</td>
    <td>-1</td>
    <td>2</td>
    <td>1</td>
    <td>3</td>
    <td>2</td>
    <td>0</td>
  </tr>
</table>

```
capacity: 4
_numStored: 4
_head: 1
_tail: 0
```

As the capacity has been reached, the head of the list is replaced by the newly
appended `e3`.  This corresponds to the list `Seq( e2, e3, e4, e3 )`, 
stored as `[e2]` ↔ `[e3]` ↔ `[e4]` ↔ `[e3]`.
Note the changes to `_head` and `_tail`.

##### Following Line 9 execution

<table>
  <tr>
    <th colspan=2>None</th>
    <th colspan=2>Some(e2)</th>
    <th colspan=2>None</th>
    <th colspan=2>Some(e4)</th>
  </tr>
  <tr>
    <td></td>
    <td></td>
    <td>-1</td>
    <td>3</td>
    <td></td>
    <td></td>
    <td>1</td>
    <td>-1</td>
  </tr>
</table>

```
capacity: 4
_numStored: 2
_head: 1
_tail: 3
```

Both copies of `e3` are removed.  This corresponds to the list `Seq( e2, e4 )`, 
stored as `[e2]` ↔ `[e4]`
Note the changes to `_head` and `_tail`.

##### Following Line 10 execution

<table>
  <tr>
    <th colspan=2>None</th>
    <th colspan=2>Some(e2)</th>
    <th colspan=2>Some(e5)</th>
    <th colspan=2>Some(e4)</th>
  </tr>
  <tr>
    <td></td>
    <td></td>
    <td>-1</td>
    <td>3</td>
    <td>3</td>
    <td>-1</td>
    <td>1</td>
    <td>2</td>
  </tr>
</table>

```
capacity: 4
_numStored: 3
_head: 1
_tail: 2
```

The newly added `e5` may be added into either of the free slots at indices 0 and
2.  The above example inserts it into the slot at index 2.
This corresponds to the list `Seq( e2, e4, e5 )`, 
stored as `[e2]` ↔ `[e4]` ↔ `[e5]`.

##### Following Line 14 execution

<table>
  <tr>
    <th colspan=2>None</th>
    <th colspan=2>Some(e2)</th>
    <th colspan=2>Some(e5)</th>
    <th colspan=2>None</th>
  </tr>
  <tr>
    <td></td>
    <td></td>
    <td>-1</td>
    <td>2</td>
    <td>1</td>
    <td>-1</td>
    <td></td>
    <td></td>
  </tr>
</table>

```
capacity: 4
_numStored: 2
_head: 1
_tail: 2
```

The iterator remove operation removes the list node at the last index to be
returned by the `next` method (`e4` at array index 3), here.
This corresponds to the list `Seq( e2, e5 )`, 
stored as `[e2]` ↔ `[e5]`.
Note the changes to the `_next` and `_prev` pointers of the nodes holding `e2` 
and `e5` respectively.

## Suggested approach


1. Read over the tasks and make notes on what each part is supposed to do and
   what invariants it should maintain.

2. Write your tests to the extent that you feel it would be sufficient that if
   you pass all of them your work is complete.

3. Begin work on `append` and try adding values and checking that they are 
   present in the array.  You can do so with the iterator, or the initialy 
   provided unit tests.  Note that if your list isn't linked correctly, your 
   iterator may cause an infinite loop.
      * Check that your lists are updating appropriately.
      * Add more items than your backing storage array can hold to see that you
        wrap properly.
      * Use the debugger to step through method executions or use print 
        statements to log a trace through your methods to ensure that they work
        as expected.

4. Next work on the `countEntry` and `LinkedListIterator.remove` methods.

5. After this, the outer `remove` entry should be easier to approach.

6. `apply` and `update` both depend on other functionality to work correctly, 
  but may be completed at any time.

It is particularly important to follow some semblance of this approach when
working on the assignment, as it will be confusing to work out of this order.
For instance, it doesn't make sense trying to work on removal of data when 
nothing is stored.  

Be sure to test as you go.  **Don't wait until the end to test!**



## Allowed library/container usage

* The provided template already references `scala.collection.mutable.Seq` and 
  `scala.collection.Iterator`, traits, and `scala.Array`.  
* You may not any other uses of containers or traits from the scala collection 
  library.


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
* Spring 2021 - Andrew Hughes (ahughes6@buffalo.edu)
