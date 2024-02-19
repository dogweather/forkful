---
aliases:
- /hi/c-sharp/interpolating-a-string/
date: 2024-01-20 17:50:25.589944-07:00
description: "String interpolation \u090F\u0915 \u0924\u0930\u0940\u0915\u093E \u0939\
  \u0948 \u091C\u093F\u0938\u0938\u0947 \u0939\u092E variables \u0914\u0930 expressions\
  \ \u0915\u094B directly strings \u0915\u0947 \u0905\u0902\u0926\u0930 \u091C\u094B\
  \u0921\u093C \u0938\u0915\u0924\u0947 \u0939\u0948\u0902. \u092F\u0939 \u0915\u094B\
  \u0921 \u0915\u094B \u0914\u0930 \u0905\u0927\u093F\u0915 \u092A\u0920\u0928\u0940\
  \u092F \u0914\u0930 \u092E\u0947\u0902\u091F\u0947\u0928 \u0915\u0930\u0928\u0947\
  \ \u092E\u0947\u0902\u2026"
lastmod: 2024-02-18 23:09:03.320594
model: gpt-4-1106-preview
summary: "String interpolation \u090F\u0915 \u0924\u0930\u0940\u0915\u093E \u0939\u0948\
  \ \u091C\u093F\u0938\u0938\u0947 \u0939\u092E variables \u0914\u0930 expressions\
  \ \u0915\u094B directly strings \u0915\u0947 \u0905\u0902\u0926\u0930 \u091C\u094B\
  \u0921\u093C \u0938\u0915\u0924\u0947 \u0939\u0948\u0902. \u092F\u0939 \u0915\u094B\
  \u0921 \u0915\u094B \u0914\u0930 \u0905\u0927\u093F\u0915 \u092A\u0920\u0928\u0940\
  \u092F \u0914\u0930 \u092E\u0947\u0902\u091F\u0947\u0928 \u0915\u0930\u0928\u0947\
  \ \u092E\u0947\u0902\u2026"
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0907\u0902\u091F\u0930\u092A\
  \u094B\u0932\u0947\u0936\u0928"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
String interpolation एक तरीका है जिससे हम variables और expressions को directly strings के अंदर जोड़ सकते हैं. यह कोड को और अधिक पठनीय और मेंटेन करने में आसान बनाता है.

## How to: (कैसे करें:)
```C#
string name = "Rohan";
int age = 25;
// Using string interpolation
string greeting = $"Hello, {name}! You are {age} years old.";
Console.WriteLine(greeting);
```
Sample Output:
```
Hello, Rohan! You are 25 years old.
```

## Deep Dive (गहराई से जानकारी:)
String interpolation C# में C# 6.0 से शुरू हुआ था. इससे पहले, programmers `String.Format()` का इस्तेमाल करते थे, जो एक तरह से अव्यवहारिक था क्योंकि आपको placeholders का ध्यान रखना पड़ता था. Interpolation में, आप { } braces के अंदर directly expressions डाल सकते हैं.

Implementation की बात करें तो, JIT compiler आपके interpolated string को `String.Format()` में बदल देता है. आप complex expressions भी use कर सकते हैं, जैसे `$"Sum of {a} and {b} is {a + b}"`. यह C# में एक साफ, संक्षिप्त और powerful feature है.

## See Also (और देखें:)
- [String interpolation in C#](https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/tokens/interpolated)
- [C# Programming Guide](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/)
- [String.Format Method](https://docs.microsoft.com/en-us/dotnet/api/system.string.format)
