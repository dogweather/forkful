---
title:                "स्ट्रिंग से दिनांक का विश्लेषण"
html_title:           "C#: स्ट्रिंग से दिनांक का विश्लेषण"
simple_title:         "स्ट्रिंग से दिनांक का विश्लेषण"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

प्रोग्रामर्स कभी-कभी तिथि को एक स्ट्रिंग से पार्स करने की जरूरत होती है। तिथि को स्ट्रिंग से पार्स करना एक तारीख को कंप्यूटर को समझने के लिए उपयोगी होता है।

## कैसे करें:

C# काड के भीतर, हम इस उदाहरण के माध्यम से सीखेंगे कि कैसे एक स्ट्रिंग से तिथि को पार्स कर सकते हैं।

```
string date = "12/01/2021";
DateTime convertedDate = DateTime.Parse(date);
Console.WriteLine(convertedDate);
```

आउटपुट:
```
12/01/2021 12:00:00 AM
```

## गहराई तक जाइए:

तिथि को स्ट्रिंग से पार्स करने की प्रक्रिया पिछले कुछ सालों में काफी आसान हो गई है। पहले, प्रोग्रामर्स को अपने कोड में तिथि फॉर्मेट को मैन्युअली सेट करना होता था। लेकिन अब, वे सिर्फ एक फ़ंक्शन का उपयोग करके स्ट्रिंग से तिथि को पार्स कर सकते हैं। अन्य विकल्पों में, कुछ प्रोग्रामिंग भाषाओं में यह काम करना कठिन हो सकता है जिससे समय और संसाधन बचाया जा सकता है। संसाधनों के साथ उपयोगर्ताओं को समय के दौरान परेशानियों से बचाने के लिए तिथि को स्ट्रिंग से पार्स करने एक उपयोगी तकनीक है।

## देखें भी:

अधिक जानकारी के लिए निम्नलिखित स्रोतों को देखें:
- [How to: Convert String to DateTime](https://docs.microsoft.com/en-us/dotnet/standard/base-types/how-to-convert-a-string-to-a-datetime)
- [DateTime.Parse Method](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.parse?redirectedfrom=MSDN&view=net-5.0#overloads)
- [Parsing Date and Time Strings in .NET](https://www.codeproject.com/Articles/14743/Parsing-Date-and-Time-Strings-in-NET)