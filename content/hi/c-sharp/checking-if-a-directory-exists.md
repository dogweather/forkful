---
title:                "डायरेक्टरी मौजूद है या नहीं जांच करना"
html_title:           "C#: डायरेक्टरी मौजूद है या नहीं जांच करना"
simple_title:         "डायरेक्टरी मौजूद है या नहीं जांच करना"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Kyun

Kya aapne kabhi directory ki maujoodgi ko check karne ka anubhav kiya hai? Agar haan, to aap jaante honge ki directory ki maujoodgi ko check karna kyon mahatvapurna hai. Yeh humare code mein error aur bugs ko avoid karne mein madad karta hai aur sahi functionality ka assurance deta hai.

## Kaise

```C#
if(Directory.Exists(@"C:\Users\Username\Documents"))
{
    Console.WriteLine("Directory exists!");
}
else
{
    Console.WriteLine("Directory does not exist.");
}
```

Jab hum `Directory.Exists` function ko use karte hain, humein path provide karna hota hai jiske baare mein hume jaanna hai ki wah maujood hai ya nahi. Agar directory maujood hai, function `true` return karega aur hume "Directory exists!" ka message milega. Agar directory nahi maujood hai, function `false` return karega aur hume "Directory does not exist." ka message milega. 

## Deep Dive

Directory ki maujoodgi ko check karne ka process bahut hi simple hai. Yeh `Directory` class mein ek built-in function hai jo `bool` data type return karta hai. Is function ko use karne se pehle humein `using System.IO;` namespace ko include kar lena chahiye. 

Is process mein hum `Path` class se bhi madad le sakte hain, jiske through hum directory ka path validate kar sakte hain. Humein sirf `Directory.Exists` function use karna hota hai aur wah path ko check karta hai. Agar aapko aur zyada flexiblity chahiye, to aap `DirectoryInfo` class ka use kar sakte hain jo hume directory ki details provide karta hai.

## Dekhiye Bhi

- [Microsoft Docs: Directory.Exists Method (System.IO)](https://docs.microsoft.com/en-us/dotnet/api/system.io.directory.exists?view=netcore-3.1)
- [C# Corner: How To Check If Directory Exists in C#](https://www.c-sharpcorner.com/article/how-to-check-if-directory-exists-in-C-Sharp/)