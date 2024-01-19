---
title:                "स्ट्रिंग का अंतर्कलन"
html_title:           "Arduino: स्ट्रिंग का अंतर्कलन"
simple_title:         "स्ट्रिंग का अंतर्कलन"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/interpolating-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

स्ट्रिंग इंटरपोलेशन C# में एक technique है जिससे हम expressions को स्ट्रिंग्स के बीच में एम्बेड कर सकते हैं। यह programmers को आसानी से पढ़ने और समझने योग्य complex string formatting और data binding operation बनाने में मदद करता है।

## कैसे करें:

```C#
string name = "Amit";
string intro = $"नमस्ते, मेरा नाम {name} है।";
Console.WriteLine(intro);
```

उपरोक्त कोड का Output होगा,

```
नमस्ते, मेरा नाम Amit है।
```

आप इसे किसी भी expression के साथ उपयोग कर सकते हैं, जैसा कि निम्नलिखित कोड में दिखाया गया है,

```C#
int a = 10, b = 20;
string sum = $"योग: {a + b}";
Console.WriteLine(sum);
```

आउटपुट होगा,

```
योग: 30
```

## गहराई से जानें:

स्ट्रिंग इंटरपोलेशन C# 6.0 में शामिल किया गया था। इससे पहले, programmers को इसके लिए `String.Format` method का use करना पड़ता था, जिसे अब deprecated माना जाता है। 

स्ट्रिंग इंटरपोलेशन का एक और विकल्प है concatenation, लेकिन स्ट्रिंग इंटरपोलेशन के संगठन और पढ़ने में आसानी की वजह से यह एक बेहतर विकल्प माना जाता है। 

स्ट्रिंग इंटरपोलेशन की implementation विशेषताओं में $ symbol का उपयोग होता है और यह compile-time में String.Format कॉल के रूप में बदल जाता है। 

## देखने के लिए भी:

1. [C# गाइड - स्ट्रिंग इंटरपोलेशन Microsoft का आधिकारिक डॉक्यूमेंटेशन](https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/tokens/interpolated)
2. [Microsoft's String.Format मेथड की जानकारी](https://docs.microsoft.com/en-us/dotnet/api/system.string.format?view=net-6.0)
3. [C# के लिए बेहतरीन practices का मार्गदर्शन](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/inside-a-program/coding-conventions)