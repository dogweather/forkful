---
title:                "स्ट्रिंग को जोड़ना"
date:                  2024-01-20T17:34:40.935271-07:00
model:                 gpt-4-1106-preview
simple_title:         "स्ट्रिंग को जोड़ना"

category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
स्ट्रिंग्स को जोड़ना मतलब है कि एक के बाद एक स्ट्रिंग्स को एक साथ मिलाना। ये काम क्यों? क्योंकि हमें कभी-कभी अलग-अलग डेटा हिस्सों को साथ में पेश करने की ज़रूरत पड़ती है।

## How to: (कैसे करें:)
```C#
string firstName = "राज";
string lastName = "कुमार";
string fullName = firstName + " " + lastName; // Concatenation using +
Console.WriteLine(fullName); // आउटपुट: राज कुमार

string greeting = "नमस्ते, ";
string message = greeting + fullName + "!"; // Another example
Console.WriteLine(message); // आउटपुट: नमस्ते, राज कुमार!

// Using String.Format
string formatted = String.Format("मेरा नाम {0} है।", fullName);
Console.WriteLine(formatted); // आउटपुट: मेरा नाम राज कुमार है।

// Using interpolation
string interpolated = $"मेरा नाम {fullName} है।";
Console.WriteLine(interpolated); // आउटपुट: मेरा नाम राज कुमार है।
```

## Deep Dive (गहराई में जानकारी)
स्ट्रिंग जोड़ना प्रोग्रामिंग के शुरूआती दिनों से ही है। पहले इसे सीधे '+' ऑपरेटर से किया जाता था, जैसे `string1 + string2`. जैसे-जैसे C# विकसित हुआ, नए तरीके आए जैसे `String.Format` और स्ट्रिंग इंटरपोलेशन जिसे उपर उदाहरण में दिखाया गया।

ऐतिहासिक रूप से, '+' ऑपरेटर के ज्यादा इस्तेमाल से परफॉरमेंस मुद्दे आ सकते थे, खासकर बड़ी स्ट्रिंग्स के लिए। फिर StringBuilder क्लास आई, जिसने एफिशिएंसी में सुधार किया।

विकल्पों के रूप में हमारे पास अब StringBuilder, String.Format और स्ट्रिंग इंटरपोलेशन है। इंटरपोलेशन सिंटैक्स सबसे साफ और आसान है।

कुछ अंतर्निहित लागतें हैं जब हम स्ट्रिंग्स को जोड़ते हैं। जब '+' का इस्तेमाल होता है, हर जोड़ के लिए नई स्ट्रिंग बनती है, क्योंकि स्ट्रिंग्स C# में immutable होती हैं। इसीलिए बड़े डेटा होने पर StringBuilder इस्तेमाल करना बेहतर रहता है।

## See Also (और भी जानकारी)
- [String concatenation in C# (Microsoft Docs)](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/strings/how-to-concatenate-multiple-strings)
- [String interpolation in C# (Microsoft Docs)](https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/tokens/interpolated)
- [StringBuilder Class (Microsoft Docs)](https://docs.microsoft.com/en-us/dotnet/api/system.text.stringbuilder?view=netcore-3.1)
