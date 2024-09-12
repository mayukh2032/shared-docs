Here’s a structured teaching material outline to guide you in teaching Python to someone with COBOL experience:


---

Lesson Plan: Transitioning from COBOL to Python

Module 1: Introduction to Python

1.1 What is Python?

Overview of Python: Its simplicity and use cases.

Differences between Python and COBOL.

Python installation and setting up the environment (IDLE, Jupyter Notebook, etc.).


1.2 Python Syntax Basics

Variables: No need for type declarations (compared to COBOL).

name = "Alice"
age = 60

Comments: Single-line and multi-line comments.
COBOL: *
Python: # for single line, ''' for multi-line.

# This is a comment
'''
This is a multi-line comment
'''


1.3 Indentation in Python

Unlike COBOL, Python relies on indentation to define blocks of code.
COBOL: IF/END-IF.
Python:

if age > 60:
    print("Senior Citizen")



---

Module 2: Data Types and Variables

2.1 Basic Data Types

Numbers: Integers and floats.

Strings: Creating and manipulating strings.
COBOL: Using DISPLAY and ACCEPT.
Python:

name = "John"
print(f"Hello, {name}")


2.2 Data Collections

Lists (similar to arrays in COBOL):

fruits = ["apple", "banana", "cherry"]

Dictionaries (similar to tables with keys and values):

person = {"name": "John", "age": 60}



---

Module 3: Control Structures

3.1 Conditional Statements

COBOL: IF/END-IF blocks.

Python:

if salary > 50000:
    print("High salary")
else:
    print("Average salary")


3.2 Loops

COBOL: PERFORM loop.

Python: for and while loops.

for i in range(5):
    print(i)



---

Module 4: Functions

4.1 Defining and Calling Functions

COBOL uses sections and paragraphs. Python uses functions.

def greet(name):
    return f"Hello, {name}"

print(greet("John"))


4.2 Parameters and Return Values

Similar to CALL and RETURNING in COBOL.



---

Module 5: File Handling

5.1 Reading from a File

COBOL: OPEN INPUT file-name, READ file-name INTO.

Python:

with open('file.txt', 'r') as file:
    data = file.read()
    print(data)


5.2 Writing to a File

COBOL: OPEN OUTPUT file-name, WRITE record-name.

Python:

with open('file.txt', 'w') as file:
    file.write("Hello, World")



---

Module 6: Libraries and Modules

6.1 Importing Libraries

COBOL has predefined subroutines. Python has libraries.

import math
print(math.sqrt(16))


6.2 Using External Libraries (like pandas, csv)

Show how to use popular libraries for file handling or data analysis.



---

Module 7: Real-World Example (COBOL to Python)

7.1 Converting COBOL Program to Python

Take a simple COBOL program (e.g., one that reads a file, processes data, and outputs it).

Translate that into Python.


Example COBOL Program:

IDENTIFICATION DIVISION.
       DATA DIVISION.
       FILE-CONTROL.
       SELECT INPUT-FILE ASSIGN TO "input.txt".
       PROCEDURE DIVISION.
       OPEN INPUT INPUT-FILE.
       READ INPUT-FILE.
       DISPLAY "File content".
       CLOSE INPUT-FILE.
       STOP RUN.

Translated Python Program:

with open('input.txt', 'r') as file:
    data = file.read()
    print("File content:", data)


---

Module 8: Final Project

A small project combining reading data from a file, processing it, and writing output to another file.



---

Additional Resources:

1. Books:

"Automate the Boring Stuff with Python" (great for learning basic automation).

"Python Crash Course" by Eric Matthes.



2. Online Platforms:

W3Schools Python Tutorial

Real Python



3. Interactive Coding Platforms:

Repl.it: For running Python code directly in the browser.

Jupyter Notebooks: Great for interactive learning.





---

Feel free to adapt this material depending on the person’s pace of learning!

