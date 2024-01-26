---
title:                "स्ट्रिंग से दिनांक पार्स करना"
date:                  2024-01-20T15:35:37.012310-07:00
html_title:           "Arduino: स्ट्रिंग से दिनांक पार्स करना"
simple_title:         "स्ट्रिंग से दिनांक पार्स करना"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
तारीख को स्ट्रिंग से पार्स करना मतलब है कि एक टेक्स्ट फॉर्मैट में दिए गए डेट को डेटटाइप में बदलना। प्रोग्रामर्स यह इसलिए करते हैं क्योंकि यूजर्स अक्सर डेट इनपुट स्ट्रिंग के रूप में देते हैं और सिस्टम को उसे समझने के लिए पार्स करना पड़ता है।

## How to: (कैसे करें:)
```C#
using System;
using System.Globalization;

class Program
{
    static void Main()
    {
        string dateString = "31-12-2023"; // DD-MM-YYYY format
        DateTime parsedDate;

        if(DateTime.TryParseExact(dateString, "dd-MM-yyyy", 
                                  CultureInfo.InvariantCulture, 
                                  DateTimeStyles.None, out parsedDate))
        {
            Console.WriteLine("Parsing successful!");
            Console.WriteLine(parsedDate.ToString("dd/MM/yyyy")); // Output in different format
        }
        else
        {
            Console.WriteLine("Parsing failed.");
        }
    }
}
```

## Deep Dive (गहन जानकारी)
स्ट्रिंग से डेट पार्स करने की हिस्ट्री C# लैंग्वेज के शुरुआत से है। `DateTime.Parse()` फंक्शन से लेकर `DateTime.TryParseExact()` तक, C# ने विभिन्न तरीके प्रदान किए हैं। विकल्पों में `DateTimeOffset`, `CultureInfo`, `DateTimeStyles` आते हैं और पर्सनग कस्टमाइजेशन के लिए फॉर्मैट स्ट्रिंग्स होते हैं। इम्प्लीमेंटेशन में लोकेल और कल्चर सेंसटिविटी महत्वपूर्ण हैं, क्योंकि समय और तारीख का प्रारूप भौगोलिक स्थान के अनुसार बदलता रहता है।

## See Also (देखें भी)
- [DateTime.TryParseExact Method](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.tryparseexact?view=netframework-4.8)
- [Custom date and time format strings](https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings)
- [CultureInfo Class](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.cultureinfo?view=netframework-4.8)
