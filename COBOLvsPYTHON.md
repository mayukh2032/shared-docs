---

## Lesson Plan: Transitioning from COBOL to Python


---

Module 1: Introduction to Python

1.1 What is Python?

History & Popularity:

Python was created by Guido van Rossum and is known for its readability and simple syntax.

Widely used in web development, automation, data science, and machine learning.

Comparison to COBOL: COBOL is predominantly used in business, finance, and administrative systems. Python is more versatile across industries.



1.2 Installing Python

Installation Guide: Walk through how to install Python from python.org.

Setting up IDEs:

IDLE: A simple editor for running Python code.

VS Code or PyCharm: More advanced editors.

Jupyter Notebooks: Interactive environment for experimenting with code.



1.3 Python vs. COBOL Syntax

COBOL's verbosity vs. Python's simplicity:

COBOL:

MOVE "Hello, World" TO WS-MESSAGE
DISPLAY WS-MESSAGE

Python:

message = "Hello, World"
print(message)



1.4 Python’s Strengths

Dynamic Typing: No need to declare variable types.

No Line Numbers: Unlike COBOL’s column-based structure.

High-Level Language: Python abstracts away many low-level operations like memory management.



---

Module 2: Data Types and Variables

2.1 Basic Data Types in Python

Integers (int): Whole numbers (similar to COBOL’s PIC 9).

Floating-point numbers (float): Numbers with decimal points (like COBOL's PIC 9V).

Strings (str): Text data, enclosed in single, double, or triple quotes.


Examples:

age = 60  # int
salary = 75000.50  # float
name = "John Doe"  # string

2.2 Lists, Tuples, and Sets

Lists: Dynamic arrays, similar to COBOL tables but more flexible.
COBOL array example:

01 FRUIT-TABLE.
   05 FRUIT-NAME PIC X(10) OCCURS 5 TIMES.

Python list:

fruits = ["Apple", "Banana", "Cherry", "Date", "Elderberry"]
print(fruits[0])  # Apple

Tuples: Immutable collections (once created, they cannot be changed).

coordinates = (10, 20)

Sets: Unordered, unique collections.

unique_numbers = {1, 2, 3, 3}
print(unique_numbers)  # Output: {1, 2, 3}


2.3 Dictionaries (Hash Tables in COBOL)

Dictionaries in Python are like COBOL’s indexed files.

employee = {"name": "John", "age": 60, "role": "Manager"}
print(employee["name"])  # John



---

Module 3: Control Structures

3.1 Conditional Statements

COBOL’s IF and EVALUATE:

IF AGE > 60
    DISPLAY "Senior Citizen"
END-IF.

Python’s if, elif, and else:

age = 60
if age > 60:
    print("Senior Citizen")
elif age == 60:
    print("Exactly 60")
else:
    print("Not Senior Citizen")


3.2 Loops in Python

COBOL Perform Loops:

PERFORM VARYING I FROM 1 BY 1 UNTIL I > 5
    DISPLAY I
END-PERFORM.

Python’s for and while loops:

for i in range(1, 6):
    print(i)

i = 1
while i <= 5:
    print(i)
    i += 1


3.3 Handling Exceptions

Unlike COBOL, Python handles errors gracefully using try, except, and finally blocks.

try:
    result = 10 / 0
except ZeroDivisionError:
    print("Cannot divide by zero")
finally:
    print("Completed")



---

Module 4: Functions

4.1 COBOL Subroutines vs. Python Functions

COBOL’s PERFORM vs. Python’s function calls.

PERFORM CALCULATE-SALARY.

Python equivalent:

def calculate_salary(hours, rate):
    return hours * rate

print(calculate_salary(40, 25))


4.2 Parameters and Return Values

In Python, functions can return multiple values:

def get_employee():
    return "John", 60, "Manager"

name, age, role = get_employee()



---

Module 5: File Handling

5.1 Reading from and Writing to Files

COBOL:

OPEN INPUT FILE-NAME
READ FILE-NAME INTO WS-DATA

Python:

# Reading from a file
with open('input.txt', 'r') as file:
    data = file.read()
    print(data)

# Writing to a file
with open('output.txt', 'w') as file:
    file.write("Hello, Python")


5.2 File Processing

Discuss how Python simplifies file handling with built-in libraries, and emphasize comparison to COBOL’s file handling capabilities.



---

Module 6: Python Libraries and Modules

6.1 Importing Libraries

Built-in Libraries: Python’s libraries make tasks easier, like math for mathematical operations.

import math
print(math.sqrt(25))


6.2 Using External Libraries

Introduce popular libraries like pandas for data manipulation, csv for reading CSV files, and os for system-level operations.

import pandas as pd
data = pd.read_csv('employees.csv')
print(data.head())



---

Module 7: COBOL to Python Conversion

7.1 Step-by-Step Conversion Examples

Walk through simple COBOL programs and demonstrate Python equivalents.

File reading (COBOL example previously shown).

String manipulation: COBOL:

MOVE "ABC" TO WS-VARIABLE.
STRING WS-VARIABLE DELIMITED BY SPACE
       "DEF" INTO WS-RESULT.

Python:

result = "ABC" + "DEF"
print(result)




---

Module 8: Final Project - Data Processing System

8.1 Problem Statement

Create a simple payroll system where the user can input employee details, store them in a file, and calculate total pay based on working hours.


8.2 Key Concepts to Apply

File handling: Read and write employee details.

Functions: Calculate salary based on input.

Loops and conditionals: Manage multiple employees.



---

Module 9: Advanced Topics (Optional)

9.1 Object-Oriented Programming

Introduce classes and objects in Python if the learner is comfortable with procedural Python.

class Employee:
    def __init__(self, name, age):
        self.name = name
        self.age = age

    def display_info(self):
        print(f"Name: {self.name}, Age: {self.age}")

emp = Employee("John", 60)
emp.display_info()


9.2 Data Analysis with Pandas (Optional)

Introduce them to data manipulation and analysis using the pandas library, especially if they work with data processing in COBOL.



---

Additional Learning Tools

Python Documentation: Python Official Docs

Interactive Python Practice:

Repl.it

Kaggle


Cheat Sheets:

Provide a simple COBOL-to-Python translation cheat sheet for common syntax.




---

This expanded material provides a more comprehensive learning experience, covering both fundamental concepts and advanced topics that are likely to be relevant for someone transitioning from COBOL.

