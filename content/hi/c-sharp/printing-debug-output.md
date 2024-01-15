---
title:                "डिबग आउटपुट छापना"
html_title:           "C#: डिबग आउटपुट छापना"
simple_title:         "डिबग आउटपुट छापना"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Kyu
Kya aap kabhi apne C# code ke saath debugging kiya hai? Agar haan, toh aapne shayad debug output ka istemal kiya hoga. Debug output, ya fir console output, ek aasan aur zaroori tarika hai apne code ke har step ko samajhne ka. Is article mein hum baat karenge debug output ka istemal karna kyu zaroori hai.

## Kaise Kare
Debug output ka istemal karna C# mein kaafi aasaan hai. Sabse pehle, aapko ```Console.WriteLine()``` method ka istemal karna hai. Is method mein aapko kuch bhi print karna hai apne console mein, jaise ki variables ke values, messages, ya fir error messages.

Maan lijiye aapko ek program banana hai, jisme aap user se ek number input lete hain aur us number ka square calculate karke print karte hain. Is situation mein, aap debugging ke liye output print kar sakte hain, jaise ki:

```C#
int num = Convert.ToInt32(Console.ReadLine());
Console.WriteLine("Input number: {0}", num);
int square = num * num;
Console.WriteLine("Square: {0}", square);
```

Is code mein, humne ```Console.WriteLine()``` method ko 2 baar use kiya hai. Pehle, humne input number ko print kiya hai, taaki hume pata chal sake kis number ka square hum calculate kar rahe hain. Fir, humne square ka value print kiya hai.

Ek aur tareeka debug output ka istemal karne ka hota hai ```Debug.WriteLine()``` method. Is method ko istemal karne ke liye, aapko System.Diagnostics namespace ko ```using``` statement ke saath include karna hoga.

## Deep Dive
Debugging output ka istemal karne ka ek ke hazaar tareeko hote hain, lekin sabka ultimate goal ek hi hota hai - apne code ko debug karne ka aasan aur efficient tareeka. Debug output ka istemal karke, hum apne code ke errors aur bugs ko pakad sakte hain aur unhe fix kar sakte hain. Isse humara code stable aur reliable ban jata hai.

Ek important point hai debug output ka istemal karna production code mein. Production code mein, debug output ka istemal avoid karna chahiye kyuki isse code ka execution slow ho sakta hai. Isliye, debug output ka istemal sirf development aur testing phase mein hi karna chahiye.

## Dekhiye Bhi
Agar aapko C# aur debugging se judi aur articles padhna hai, toh yeh links aapke kaam aasakte hain:
1. [C# basics for beginners](https://www.tutorialspoint.com/csharp/index.htm)
2. [Debugging in C#](https://docs.microsoft.com/en-us/visualstudio/debugger/?view=vs-2019)
3. [Best practices for debugging in C#](https://stackify.com/csharp-debugging-tips-tricks/)