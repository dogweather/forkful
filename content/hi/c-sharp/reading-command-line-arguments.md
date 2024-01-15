---
title:                "कम्प्यूटर प्रोग्रामिंग पर एक लेख का शीर्षक: कमांड लाइन आर्ग्यूमेंट्स को पढ़ना"
html_title:           "C#: कम्प्यूटर प्रोग्रामिंग पर एक लेख का शीर्षक: कमांड लाइन आर्ग्यूमेंट्स को पढ़ना"
simple_title:         "कम्प्यूटर प्रोग्रामिंग पर एक लेख का शीर्षक: कमांड लाइन आर्ग्यूमेंट्स को पढ़ना"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Kyu:

Kya aapko command line arguments ke baare mein jaankari hai? Yeh computer programming mein ek important concept hai aur iska use kiya jaata hai user input ko program mein dene ke liye. Agar aap C# mein kaam kar rahe hai, toh yeh article aapke liye hai. Iss article mein hum command line arguments ke baare mein baat karenge, kyuki yeh ek useful technique hai jise har programmer ko jaan lena chahiye.

## Kaise Kare:

Command line arguments ko C# mein read karna bahut hi aasan hai. Hamara program ek simple console application hoga jo user se argument input lega aur usse print kar dega. Chaliye dekhte hai kaise aap ise code mein implement kar sakte hai:

```C#
using System;

// Main function
public static int Main(string[] args)
{
    // Loop through command line arguments
    for (int i = 0; i < args.Length; i++) 
    { 
        Console.WriteLine("Argument {0}: {1}", i+1, args[i]);  
    } 
    return 0;
}
```

Jaise aap is code ko run karenge, aapke screen par command line arguments ki list print hogi. Samajhne ke liye, hum ek example dekhte hai. Agar aap command line mein `Hello World` type karke run karte hai, toh program print karega: Argument 1: Hello and Argument 2: World. Aap dekh sakte hai ki humare program ne har word ko alag-alag argument ka form diya hai.

## Deep Dive:

Command line arguments ko read karne ke liye, hum string array ka use karte hai. Is array mein saare command line arguments store ho jate hai aur hum unhe access kar sakte hai. Agar aapko sirf specific arguments ki value chahiye, toh aap unhe index se access kar sakte hai. Jaise: `args[0]` aapke program ka first argument hoga.

Iske alawa, hum `args.Length` ka use karke array ki length bhi check kar sakte hai. Isse hume pata chalta hai ki kitne arguments user ne diye hai. Iss information ka use hum apne program mein kar sakte hai aur use accordingly modify kar sakte hai.

Is tarah se command line arguments ek bahut hi useful tool hai jise aap apne C# programs mein use kar sakte hai. Agar aap is concept ke bare mein aur jaankari chahte hai, toh aap online tutorials aur documentation se bhi help le sakte hai.

## Dekhe Bhiye:

Agar aapko C# mein aur bhi programming concepts aur tutorials chahiye, toh yeh links aapke kaam aayenge:

- [C# Tutorials](https://docs.microsoft.com/en-us/dotnet/csharp/tour-of-csharp/)
- [C# Documentation](https://docs.microsoft.com/en-us/dotnet/csharp/)
- [Introduction to Command Line Arguments in C#](https://www.c-sharpcorner.com/article/introduction-to-command-line-arguments-in-C-Sharp/)