---
title:                "भविष्य या अतीत में तारीख की गणना"
aliases:
- /hi/c-sharp/calculating-a-date-in-the-future-or-past/
date:                  2024-01-20T17:31:54.625688-07:00
model:                 gpt-4-1106-preview
simple_title:         "भविष्य या अतीत में तारीख की गणना"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
तारीख का भविष्य या अतीत में हिसाब करना यानी वर्तमान तिथि से कुछ दिन, महिने या साल जोड़ना या घटाना। प्रोग्रामर्स यह इसलिए करते हैं क्योंकि कई एप्लीकेशन्स में समय-सीमा, कार्यकाल और रिमाइंडर्स के प्रबंधन की ज़रुरत होती है।

## कैसे करें:
```C#
using System;

class Program {
    static void Main() {
        DateTime today = DateTime.Now;
        // 10 दिन भविष्य में
        DateTime tenDaysLater = today.AddDays(10);
        // 10 दिन अतीत में
        DateTime tenDaysBefore = today.AddDays(-10);

        Console.WriteLine("आज की तारीख: " + today.ToShortDateString());
        Console.WriteLine("10 दिन बाद की तारीख: " + tenDaysLater.ToShortDateString());
        Console.WriteLine("10 दिन पहले की तारीख: " + tenDaysBefore.ToShortDateString());
    }
}
```
उदाहरण आउटपुट:
```
आज की तारीख: 12/3/2023
10 दिन बाद की तारीख: 22/3/2023
10 दिन पहले की तारीख: 2/3/2023
```

## गहराई से जानकारी:
`.NET` फ्रेमवर्क का उपयोग करते हुए तारीखों का प्रबंधन आसान हो जाता है। `DateTime` क्लास 1 जनवरी 0001 से 31 दिसम्बर 9999 तक की तारीखें संभाल सकती है। प्रोग्रामर्स `AddDays`, `AddMonths`, `AddYears`, जैसे मेथड्स का उपयोग कर तारीखों को मैनेज करते हैं। 

अल्टरनेटिव्स के रूप में `NodaTime` जैसे लाइब्रेरीज भी मौजूद हैं जो अधिक जटिल समय परिप्रेक्ष्यों को संभालते हैं। इनका उपयोग करके, टाइम ज़ोन्स और डेलाइट सेविंग्स से जुड़े मुद्दों को बेहतर तरीके से हल किया जा सकता है।

बेशक, किसी भी तिथि गणित में लीप साल और टाइम ज़ोन्स जैसे मुद्दे होते हैं जिनको संजीदगी से सोच-समझकर संभालना पड़ता है। `DateTimeOffset` और `TimeZoneInfo` क्लासेस इन मुद्दों का ध्यान रखते हैं।

## संबंधित सूत्र (See Also):
- Microsoft Docs पर `DateTime` संरचना: [https://docs.microsoft.com/en-us/dotnet/api/system.datetime](https://docs.microsoft.com/en-us/dotnet/api/system.datetime)
- `NodaTime` लाइब्रेरी: [https://nodatime.org](https://nodatime.org)
- टाइम ज़ोन्स का हैंडलिंग: [https://docs.microsoft.com/en-us/dotnet/standard/datetime/choosing-between-datetime](https://docs.microsoft.com/en-us/dotnet/standard/datetime/choosing-between-datetime)
