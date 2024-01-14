---
title:    "C#: डीबग आउटपुट प्रिंट करना"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Kyun

Debug output print karna aapke programming kaafi helpful ho sakta hai, kyunki ye aapko apne code mein kuch galtiyaan aur chhoti-chhoti problems ko dhoondne mein madad karega. Isse aap apne code ko debug karte samay aasani se track kar sakte hain aur sahi solutions nikal sakte hain.

## Kaise

Debugging output print karna C# mein kaafi simple hai. Aap bas ek console application mein ye code add kar sakte hain:

```C#
Console.WriteLine("Debug output print karna hai");
```

Isse aapko apne code mein ye message print ho jayega. Aap apne code mein bhi variables aur different values ko print kar sakte hain, jisse aapko aur bhi detailed information milegi.

```C#
int a = 5;
string b = "Hello World!";
Console.WriteLine("a ki value = " + a);
Console.WriteLine("b ki value = " + b);
```

Is tarah se aap apne code mein jo bhi information print karna chahte hain, kar sakte hain.

## Deeper Dive

Debugging output print karna sirf code ki problems ko dhoondhne ke liye useful nahi hai, balki ye aapke code ki performance ko improve karne mein bhi madad karta hai. Aap apne code mein different stages par debug output print karke ye check kar sakte hain ki code ka kis part mein time zyada lag raha hai aur use optimize kar sakte hain.

Iske alawa, aap apne debugging output ko file mein bhi save kar sakte hain. Aisa karne se aapko apne code ka detailed log bhi mil jayega, jisse aap future mein bhi kisi problem ko solve karne mein help milegi.

## Dekhiyega

Agar aapko aur bhi C# programming ke tips aur tricks chahiye toh neeche diye gaye links ko check karein:

- [MSDN: Debugging in C#](https://docs.microsoft.com/en-us/visualstudio/debugger/debugging-in-visual-studio?view=vs-2019)
- [C# Corner: Debugging in C#](https://www.c-sharpcorner.com/article/debugging-in-c-sharp/)
- [TutorialsTeacher: Debugging in C#](https://www.tutorialsteacher.com/csharp/csharp-debugging)