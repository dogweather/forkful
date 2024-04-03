---
date: 2024-01-20 17:34:40.935271-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) ."
lastmod: '2024-03-13T22:44:52.317754-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u094B \u091C\u094B\
  \u0921\u093C\u0928\u093E"
weight: 3
---

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
