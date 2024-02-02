---
title:                "Using associative arrays"
date:                  2024-02-01T13:31:57.499701-07:00
model:                 gpt-4-0125-preview
simple_title:         "Using associative arrays"
tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/vba/using-associative-arrays.md"
---

{{< edit_this_page >}}

## What & Why?

Associative arrays, in the world of Visual Basic for Applications (VBA), let you pair keys with values in a way that makes retrieving data intuitive and fast. Programmers use them because they’re great for storing and accessing data that's logically related, like usernames and passwords, without involving complex structures or database systems.

## How to:

In VBA, associative arrays are typically implemented using the Dictionary object. Here's a quick dive into how you can use them:

First, you need to make sure you have access to the Dictionary class. Usually, you do this by setting a reference to the Microsoft Scripting Runtime library. Head over to Tools > References in the VBA editor, and check “Microsoft Scripting Runtime”.

Now, let’s create a simple Dictionary and play around with it:

```basic
Dim userPasswords As Scripting.Dictionary
Set userPasswords = New Scripting.Dictionary

' Adding items
userPasswords.Add "johnDoe", "p@ssword123"
userPasswords.Add "janeSmith", "qwerty"

' Retrieving an item
Dim johnsPassword As String
johnsPassword = userPasswords.Item("johnDoe")
MsgBox johnsPassword  ' Output: p@ssword123

' Checking if a key exists
If userPasswords.Exists("janeSmith") Then
    MsgBox "Jane's password is: " & userPasswords.Item("janeSmith")
End If

' Removing an item
userPasswords.Remove "johnDoe"

' Iterating over items
Dim key As Variant
For Each key In userPasswords.Keys
    MsgBox "Username: " & key & ", Password: " & userPasswords.Item(key)
Next
```

These snippets let you add, retrieve, check, remove, and iterate over items in a Dictionary, showcasing the flexibility associative arrays offer in handling dynamic data.

## Deep Dive

VBA’s Dictionary object is essentially a wrapper around the scripting dictionary from the Microsoft Scripting Runtime. While it’s not a native VBA feature, it’s become the de facto method for creating associative arrays due to its simplicity and efficiency.

Historically, before Dictionary became widely used, programmers might have resorted to creating custom solutions to mimic associative arrays, such as parallel arrays or using Collections with complex key-management schemes. However, these methods were typically more cumbersome and less efficient, especially with larger datasets.

While Dictionary serves most needs well, it's worth noting that more complex applications might benefit from database systems or even transitioning to more robust programming environments that support native associative arrays or hash maps efficiently, like Python or JavaScript.

For VBA programmers, though, the Dictionary remains a highly accessible and useful tool for tasks that require associative arrays, balancing ease of use with performance in most typical use cases.
