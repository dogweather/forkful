---
title:                "Organizing code into functions"
date:                  2024-02-01T13:31:41.130386-07:00
model:                 gpt-4-0125-preview
simple_title:         "Organizing code into functions"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/vba/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## What & Why?

Organizing code into functions is about slicing your code into reusable pieces - think of it as divvying up a pizza. It's nifty for keeping your code cleaner and avoiding repetition, because who likes typing the same stuff over and over?

## How to:

In VBA, functions are your best pals for making code manageable and re-usable. Let's see how to whip up a simple function that adds two numbers:

```Visual Basic for Applications
Function AddTwoNumbers(number1 As Integer, number2 As Integer) As Integer
    AddTwoNumbers = number1 + number2
End Function

Sub TestAddFunction()
    Dim result As Integer
    result = AddTwoNumbers(5, 10)
    MsgBox "The result is: " & result
End Sub
```

Running `TestAddFunction` displays a message box with "The result is: 15". Neat, right? Functions are not just about adding numbers; they can handle text, crunch data, and more. Let's do something slightly fancier:

```Visual Basic for Applications
Function GreetUser(name As String) As String
    GreetUser = "Hello, " & name & "!"
End Function

Sub TestGreetUser()
    MsgBox GreetUser("Alex")
End Sub
```

Pop `TestGreetUser` into action, and you get a friendly "Hello, Alex!" greeting in a message box.

## Deep Dive

Functions in VBA are as old as hills but still as gold as ever. They stem from the early days of procedural programming, where breaking down code into manageable chunks was revolutionary. Nowadays, in the era of object-oriented and functional programming, functions in VBA might look a bit archaic but they're still darn useful in the VBA landscape.

Despite its age, VBA doesn't fully support newer paradigms like lambdas or closures found in modern languages. This limitation means VBA functions are a bit more straightforward - you input something, process it, and output something else, no side effects or hidden states. 

In the broader programming world, there are fresher and more powerful ways to organize and reuse code, such as classes, modules, and libraries in languages like Python or Java. However, when slinging code in Excel, Word, or other Office applications, VBA's approach to functions is usually more than enough to get the job done efficiently and effectively.
