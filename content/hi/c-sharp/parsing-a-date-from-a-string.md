---
title:                "एक स्ट्रिंग से तारीख पार्स करना"
html_title:           "C++: एक स्ट्रिंग से तारीख पार्स करना"
simple_title:         "एक स्ट्रिंग से तारीख पार्स करना"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

एक string से तारीख का पार्स करना मतलब होता है तारीख का डाटा जो टेक्स्ट (string) फॉर्मेट में है, उसे डेट फॉर्मेट में कन्वर्ट करना। यह तब करते हैं जब हमें टेक्स्ट फॉर्म की तारीख को किसी कम्प्यूटेशनल कार्य के लिए या किसी और तारीख, समय फंक्शन्स के साथ तुलना करने की जरूरत होती है।

## कैसे :

हम `DateTime.Parse` फंक्शन का उपयोग करके kisi string को डेट ऑब्जेक्ट में पार्स कर सकते हैं।

```C#
string date = "15/08/2023";
DateTime parsedDate = DateTime.Parse(date);
Console.WriteLine(parsedDate);
```

उपरोक्त code का आउटपुट होगा:

```C#
08/15/2023 00:00:00
```

## गहराई में:

1. **ऐतिहासिक संदर्भ**: C# में `DateTime.Parse` फ़ंक्शन का पहली बार .NET Framework 1.0 में उपयोग हुआ। यह string को DateTime टाइप में पार्स करता है।

2. **विकल्प**: `DateTime.TryParse`, `DateTime.ParseExact`, और `DateTime.TryParseExact` इत्यादि प्रमुख विकल्प हैं। ये फ़ंक्शन्स भी string से date parse करने के लिए उपयोगी हैं, फ़र्क सिर्फ यह होता है कि ये कुछ विशेष प्रकरणों में उपयोग होते हैं।

3. **क्रियान्वयन विवरण**: `DateTime.Parse` method एक टेक्स्ट स्ट्रिंग को पर्स करने के लिए सिस्टेम की कल्चर सेटिंग्स का उपयोग करती है। अगर तारीख की फॉर्मेट सिस्टम के कल्चर के अनुसार नहीं है, तो एक्सेप्शन generate होगी।

## देखें भी:

- [DateTime.Parse Method (MS Docs)](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.parse?view=net-6.0)
- [.NET Date and Time Formats (MS Docs)](https://docs.microsoft.com/en-us/dotnet/standard/base-types/standard-date-and-time-format-strings)
- [DateTime.TryParse Method (MS Docs)](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.tryparse?view=net-6.0)