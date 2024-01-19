---
title:                "http अनुरोध भेजना"
html_title:           "Elixir: http अनुरोध भेजना"
simple_title:         "http अनुरोध भेजना"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## क्या और क्योंं?

HTTP अनुरोध भेजना, एक सर्वर से डेटा मांगना होता है, जो अक्सर वेबसाइट्स के लिए जानकारी प्राप्त करने के लिए किया जाता है। प्रोग्रामर्स इसे अक्सर API के साथ विभिन्न ऑपरेशन्स को संचालित करने के लिए या तथ्यों को प्राप्त करने के लिए करते हैं।

## कैसे करें:

हम हेरी HttpClient वर्ग का उपयोग कर के HttpStatusCode प्राप्त करने के लिए HTTP अनुरोध भेज सकते हैं:

```C#
using System;
using System.Net.Http;
using System.Threading.Tasks;

public class Program
{
    public static async Task Main()
    {
        using HttpClient client = new HttpClient();
        
        HttpResponseMessage response = await client.GetAsync("https://api.github.com");
        
        Console.WriteLine(response.StatusCode);
    }
}
```
उपर्युक्त कोड चलाने पर, आपको StatusCode मिलेगा जो सर्वर के जवाब को दर्शाता है। 

## गहरा गोता

1. **ऐतिहासिक संदर्भ**: HTTP का इस्तेमाल 1990 मे टिम बर्नर्स-ली द्वारा नेटवर्क प्रोटोकॉल के रूप में शुरू हुआ था। HttpClient वर्ग .NET 4.5 के साथ इंट्रोड्यूस किया गया था। 
2. **विकल्प**: अन्य शैलियाँ जैसे कि WebClient और HttpWebRequest भी हैं, लेकिन HttpClient ThreadPool का उपयोग करने में व्यवहार करता है, जो कि इसे अधिक कार्यक्षम बनाता है।
3. **अंतर्घटना विवरण**: HttpClient एक 'आपरेशनल कन्सेप्ट' है, या एक 'काम पर निभाने की निर्णयनीय संपत्ति'। यह एक पुन: प्रयोगी API है, जो केवल तब ही 'काम करता है' जब HTTP अनुरोध को निभाने का प्रयास किया जाता है। 

## अधिक जानकारी के लिए 

[.NET Documentation: HttpClient](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient?view=net-5.0)
[MSDN: How to use HttpClient in C#](https://msdn.microsoft.com/library/2s0fz14x.aspx)