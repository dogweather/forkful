---
title:                "दो तारीखों की तुलना"
html_title:           "Elixir: दो तारीखों की तुलना"
simple_title:         "दो तारीखों की तुलना"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/comparing-two-dates.md"
---

{{< edit_this_page >}}

# तारीखों की तुलना कैसे करें: C# में गाइड 

## क्या और क्यों?
तारीखों की तुलना करने से हमें दो दिनांकों के बीच का समय अंतर पता चलता है। प्रोग्रामर्स इसे हासिल करनें के लिए डेटा संग्रहण, विश्लेषण, और रिपोर्टिंग में उपयोग करते हैं। 

## कैसे करें:

```C#
using System;

class Program
{
    static void Main()
    {
        DateTime Date1 = new DateTime(2022, 08, 20);
        DateTime Date2 = new DateTime(2022, 08, 24);

        // तारीखों की तुलना करना 
        int result = DateTime.Compare(Date1, Date2);

        if (result < 0)
           Console.WriteLine("Date1 is earlier than Date2.");
        else if (result == 0)
           Console.WriteLine("Date1 is the same as Date2.");
        else
           Console.WriteLine("Date1 is later than Date2.");
    }
}
```

आउटपुट:
```C#
Date1 is earlier than Date2.
```

## गहराई में:

1. ऐतिहासिक प्रसंग: C# में DateTime का उपयोग करना तारीखों की तुलना के लिए एक मानक विधि रही है। पूर्व में, डेवलपर्स को इसे खुद से लिखने की आवश्यकता होती थी। 

2. विकल्प: आप `TimeSpan` ऑब्जेक्ट का भी उपयोग कर सकते हैं। `TimeSpan` का `TotalDays`, `TotalHours`, `TotalMinutes`, `TotalSeconds` आदि का उपयोग करके आप तारीखों का अंतर निकाल सकते हैं। 

3. कार्यान्वयन विवरण: `DateTime.Compare()` विधि दो तारीखों को तुलना करती हैं और तीसरी तारीख को देखने में सीमित नहीं होती हैं। 

## देखें भी:
1. [DateTime.Compare Method](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.compare?view=net-5.0) 
2. [TimeSpan Structure](https://docs.microsoft.com/en-us/dotnet/api/system.timespan?view=net-5.0)