---
title:                "টমল সঙ্গে কাজ করা"
date:                  2024-03-17T18:36:08.469848-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি ও কেন?

TOML, যার পূর্ণরূপ Tom's Obvious, Minimal Language, একটি ডেটা সিরিয়ালাইজেশন ফরম্যাট যা মূলত কনফিগারেশন ফাইলের জন্য ব্যবহৃত হয়। প্রোগ্রামাররা TOML ব্যবহার করে থাকে এর পাঠনীয়তা এবং ডেটা স্ট্রাকচারে সহজে ম্যাপ করার ক্ষমতার কারণে, যা বিভিন্ন প্রোগ্রামিং পরিবেশ, যেমন Visual Basic for Applications (VBA) এ অ্যাপ্লিকেশনগুলির কনফিগারেশনকে সহজ করে তোলে।

## কিভাবে:

VBA-তে TOML এর সাথে কাজ করা মানে TOML ফাইল পার্স করে কনফিগারেশন বা সেটিংসগুলি আপনার VBA প্রজেক্টে পড়া। VBA-এ TOML-এর জন্য কোনো নির্মিত সাপোর্ট নেই, তাই সাধারণত আপনি একটি পার্সার ব্যবহার করেন বা TOML ডেটা কোনো এমন ফরম্যাটে রূপান্তর করেন যেটা VBA সহজে কাজ করতে পারে, যেমন JSON বা XML। এখানে একটি সাধারণ TOML কনফিগ ফাইল ম্যানুয়ালি পার্স করার উপায় দেওয়া হল:

1. **স্যাম্পল TOML ফাইল** (`config.toml`):
```
title = "TOML উদাহরণ"

[database]
server = "192.168.1.1"
ports = [ 8000, 8001, 8002 ]
connection_max = 5000
enabled = true
```

2. **TOML পার্স করার জন্য VBA কোড**:

ধরুন TOML কনটেন্ট একটি স্ট্রিং ভ্যারিয়েবল `tomlStr`-এ পড়া হয়ে গেছে, নিম্নোক্ত VBA কোড `[database]` সেকশন পার্স করার একটি সরল উপায় দেখায়:

```vb
Function ParseTOML(tomlStr As String)
    Dim lines() As String
    lines = Split(tomlStr, vbCrLf)
    
    Dim config As Object
    Set config = CreateObject("Scripting.Dictionary")
    Dim currentSection As String
    currentSection = ""
    
    Dim i As Integer
    For i = 0 To UBound(lines)
        Dim line As String
        line = Trim(lines(i))
        If InStr(line, "[") > 0 And InStr(line, "]") > 0 Then
            currentSection = Mid(line, 2, Len(line) - 2)
            Set config(currentSection) = CreateObject("Scripting.Dictionary")
        ElseIf InStr(line, "=") > 0 Then
            Dim parts() As String
            parts = Split(line, "=")
            Dim key As String
            key = Trim(parts(0))
            Dim value As String
            value = Trim(parts(1))
            config(currentSection)(key) = value
        End If
    Next i
    
    'পার্সড ডেটা অ্যাক্সেস করার উদাহরণ
    Debug.Print "ডেটাবেস সার্ভার: "; config("database")("server")
End Function
```

3. **স্যাম্পল আউটপুট** (তাৎক্ষণিক উইন্ডো):
```
ডেটাবেস সার্ভার: 192.168.1.1
```

## গভীর ডুব

ডেভেলপার কমিউনিটিতে TOML-এর ব্যবহারিক গ্রহণযোগ্যতা একটি প্রবণতা দেখায়, যা সহজ, মানব-পাঠনীয় কনফিগারেশন ফাইলের দিকে ঝুঁকে, যা আগের XML-এর প্রচলনের বিপরীতে। TOML-এর ডিজাইন দর্শন স্পষ্ট সেমান্টিক্সের উপর জোর দিয়ে এবং সর্বনিম্ন ওভারহেডের সাথে সহজীকরণযোগ্য পার্সিংকে লক্ষ্য করে। VBA-এ, TOML সরাসরি হ্যান্ডল করা মানে ম্যানুয়াল পার্সিং বা বাহ্যিক টুলস ব্যবহার করা, যা TOML-কে আরও VBA-ফ্রেন্ডলি ফরম্যাটে রূপান্তর করে। যদিও এই ম্যানুয়াল পার্সিং পদ্ধতি একটি মৌলিক উপায় হিসেবে দেখা দেয়, ব�
