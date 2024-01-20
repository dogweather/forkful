---
title:                "एक तारीख को स्ट्रिंग में परिवर्तित करना"
html_title:           "Java: एक तारीख को स्ट्रिंग में परिवर्तित करना"
simple_title:         "एक तारीख को स्ट्रिंग में परिवर्तित करना"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# तारीख को स्ट्रिंग में बदलना: C# का इस्तेमाल करना

## क्या और क्यों?

तारीख को स्ट्रिंग में बदलना का मतलब है कि हम किसी तारीख को एक केवल-पाठ के रूप में दर्शाने में सक्षम होते हैं। हम इसे करते हैं ताकि हम तारीखों को सबसे अधिक लचीले ढंग से प्रदर्शित और मनिपुलेट कर सकें। 

## कैसे:

```C#
DateTime date = DateTime.Now; 
string dateString = date.ToString("dd-MM-yyyy"); 
Console.WriteLine(dateString);
```

उत्तर:

```
04-05-2022 //आज की तारीख
```

## गहराई जांच:

(1) ऐतिहासिक संदर्भ: C# में दिनांकों को स्ट्रिंग में बदलने की क्षमता .NET फ्रेमवर्क के पहले संस्करण के साथ ही आ गई थी। 

(2) विकल्प: `String.Format` मेथड का उपयोग करके भी तारीखों को स्ट्रिंग में परिवर्तित किया जा सकता है:

```C#
string dateString = String.Format("{0:dd-MM-yyyy}", date); 
Console.WriteLine(dateString);
```
(3) कार्यान्वयन विवरण: `ToString()` और `String.Format` मेथड `IFormattable` इंटरफ़ेस का उपयोग करते हैं जो ऑब्जेक्ट को विशिष्ट स्वरूप में प्रस्तुत करने की क्षमता प्रदान करती है। 

## यह भी देखें:

- C# DateTime वर्ग (http://msdn.microsoft.com/en-us/library/system.datetime)

- IFormattable इंटरफ़ेस (http://msdn.microsoft.com/en-us/library/system.IFormattable)

- तारीखों और समय के स्ट्रिंग प्रारूप (http://msdn.microsoft.com/en-us/library/az4se3k1.aspx)