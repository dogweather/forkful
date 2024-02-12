---
title:                "स्ट्रिंग इंटरपोलेशन"
aliases:
- /hi/c-sharp/interpolating-a-string/
date:                  2024-01-20T17:50:25.589944-07:00
model:                 gpt-4-1106-preview
simple_title:         "स्ट्रिंग इंटरपोलेशन"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/interpolating-a-string.md"
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
