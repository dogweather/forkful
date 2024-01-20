---
title:                "वर्तमान तारीख प्राप्त करना"
html_title:           "C#: वर्तमान तारीख प्राप्त करना"
simple_title:         "वर्तमान तारीख प्राप्त करना"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

C# में वर्तमान दिनांक प्राप्त करना हमारे समय / तारीख कोड का केंद्रीय हिस्सा हो सकता है। 
यह तब किया जाता है जब हमें टाइमस्टाम्प, ये जानने की आवश्यकता होती है कि कोई कार्य कब हुआ था, या हमें समय-संबंधित लॉगिंग की आवश्यकता हो।

## कैसे:

C# में, आप DateTime.Now प्रॉपर्टी का उपयोग करके वर्तमान दिनांक और समय प्राप्त कर सकते हैं।

```C#
using System;

class Program
{
    static void Main()
    {
        DateTime currentDate = DateTime.Now;
        Console.WriteLine("Current date and time: " + currentDate);
    }
}
```

चलाने पर, आपको कुछ ऐसा उत्तर मिलेगा (उत्तर उस समय पर निर्भर करेगा जब आप यह चलाते हैं):

```
Current date and time: 2/3/2023 1:34:22 PM
```

## गहराई में:

1. **ऐतिहासिक प्रसंग**: C# के पुराने संस्करणों में, हमें DateTime.Now.ToString() का उपयोग करने की आवश्यकता होती थी।
2. **विकल्प:** केवल वर्तमान दिनांक के लिए, DateTime.Today प्रॉपर्टी भी है।
3. **विन्यास विवरण**: DateTime.Now आंतरिक रूप से व्यवस्था की घड़ी से समय प्राप्त करता है और इसे DateTime ऑब्जेक्ट में रूपांतरित करता है।

## यदि आप और पढ़ना चाहते हैं:

- DateTime.Now प्रॉपर्टी ([Microsoft दस्तावेज़ीकरण](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.now?view=netframework-4.7.2))
- DateTime.Today प्रॉपर्टी ([Microsoft दस्तावेज़ीकरण](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.today?view=netframework-4.7.2))