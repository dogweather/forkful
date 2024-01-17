---
title:                "JSON के साथ काम करना"
html_title:           "C#: JSON के साथ काम करना"
simple_title:         "JSON के साथ काम करना"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/working-with-json.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
JSON के साथ काम करने का मतलब है कि हम डेटा को एक स्ट्रक्चर या फॉर्मैट में एक से दूसरे ऐसे बदल सकते हैं जो कि हमारे कंप्यूटर द्वारा समझी जा सकें। इसलिए, यह प्रोग्रामरों के लिए अत्यंत महत्वपूर्ण है क्योंकि वे अपने डेटा को अपनी पसंद के अनुसार नियंत्रित कर सकते हैं।

## कैसे करें:
```C#
using System;
using System.Text.Json;

public class Program
{
    public static void Main()
    {
        // एक JSON स्ट्रिंग बनाएं 
        string json = "{ \"name\":\"John\", \"age\":30, \"city\":\"New York\" }";

        // JSON स्ट्रिंग को ऑब्जेक्ट में परिवर्तित करें 
        Person person = JsonSerializer.Deserialize<Person>(json);

        // ऑब्जेक्ट के प्रॉपर्टी तक पहुंचें और मान प्रिंट करें
        Console.WriteLine($"Name: {person.Name}");
        Console.WriteLine($"Age: {person.Age}");
        Console.WriteLine($"City: {person.City}");
    }
}

public class Person
{
    public string Name { get; set; }
    public int Age { get; set; }
    public string City { get; set; }
}
```

आउटपुट:
```
Name: John
Age: 30
City: New York
```

## गहराई में जाईए:
JSON का निर्माण डेटा प्रतिस्थापन के लिए डिजाइन किया गया था। इसका उद्देश्य डेटा को आसानी से पाठन और लिखने के लिए एक साधारण स्ट्रक्चर में संजोयना करना था। वर्तमान में, यह एक मानक रूप में इस्तेमाल होता है और अन्य फॉर्मैटों के मुकाबले बहुत लोकप्रिय है। प्रोग्रामरों को पहले JSON स्ट्रिंग को मैनुअली पार्स करना पड़ता था, लेकिन C# 7.0 से पहले सरल टेक्निक्स देवमांडरीमेंट को परिणामस्वरूप उत्पन्न JSON के ऑब्जेक्ट में रूपांतरण करने के लिए पेश किये गये हैं।

## और देखें:
- [C# JSON सामग्री](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/concepts/serialization/json)
- [C# सीधे कोडिंग और JSON सामग्री मुख्यता](https://dotneteers.net/blogs/vbandi/archive/2009/09/20/StraightCodingOnTheMainframe.aspx)