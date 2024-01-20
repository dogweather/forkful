---
title:                "एक वेब पेज डाउनलोड करना"
html_title:           "Kotlin: एक वेब पेज डाउनलोड करना"
simple_title:         "एक वेब पेज डाउनलोड करना"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## क्या और क्यों? 

वेब पेज डाउनलोड करना मतलब उसकी सामग्री को अपने कंप्यूटर पर कॉपी करना। प्रोग्रामर्स इसे डाटा विश्लेषण, वेब स्क्रेपिंग या ऑफलाइन पठन के लिए करते हैं।

## कैसे करें:

वेब पेज डाउनलोड करने के लिए C# का उपयोग करते हुए कुछ कोड का उदाहरण यहां दिया गया है:

```C#
using System;
using System.Net.Http;
using System.Threading.Tasks;

class Program
{
    private static readonly HttpClient client = new HttpClient();

    static async Task Main()
    {
        var responseString = await client.GetStringAsync("http://example.com");

        Console.WriteLine(responseString);
    }
}
```

आपको ऊपर दी गई URL के स्थान पर अपनी URL डालनी होगी। यह कोड HTTP अनुरोध करेगा और सर्वर का प्रतिक्रिया प्राप्त करेगा।

## गहरी जानकारी:

वेब पेज डाउनलोड करने की क्षमता वेब ब्राउज़र के विकास के साथ ही कई दशक पुरानी है। इसके विकल्प में अन्य प्रोग्रामिंग भाषाओं का उपयोग भी हो सकता है, जैसे कि Python के `requests` लाइब्रेरी। हालांकि, C# में HttpClient class बहुत सक्षम और लचीली होती है। यह asynchronous operations का समर्थन करती है, जिससे एप्लीकेशन के प्रदर्शन में सुधार होता है।

## और भी देखें:

वेब पेज डाउनलोड करने के बारे में और जानने के लिए नीचे दिए गए संसाधनों की जाँच करें:

- [HttpClient Class in .NET](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient)
- [Web Scraping with C#](https://www.pluralsight.com/guides/web-scraping-with-csharp)
- [Asynchronous Programming in C#](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/concepts/async/)