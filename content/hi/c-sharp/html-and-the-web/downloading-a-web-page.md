---
date: 2024-01-20 17:43:40.134412-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) C# \u0915\
  \u093E \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u0915\u0930\u0915\u0947\
  , \u0906\u092A HttpClient \u0915\u094D\u0932\u093E\u0938 \u0915\u0947 \u0938\u093E\
  \u0925 \u092C\u0939\u0941\u0924 \u0906\u0938\u093E\u0928\u0940 \u0938\u0947 \u0935\
  \u0947\u092C \u092A\u0947\u091C \u0921\u093E\u0909\u0928\u0932\u094B\u0921 \u0915\
  \u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\u0964."
lastmod: '2024-04-05T21:53:54.328467-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) C# \u0915\u093E \u0907\
  \u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u0915\u0930\u0915\u0947, \u0906\u092A\
  \ HttpClient \u0915\u094D\u0932\u093E\u0938 \u0915\u0947 \u0938\u093E\u0925 \u092C\
  \u0939\u0941\u0924 \u0906\u0938\u093E\u0928\u0940 \u0938\u0947 \u0935\u0947\u092C\
  \ \u092A\u0947\u091C \u0921\u093E\u0909\u0928\u0932\u094B\u0921 \u0915\u0930 \u0938\
  \u0915\u0924\u0947 \u0939\u0948\u0902\u0964."
title: "\u0935\u0947\u092C \u092A\u0947\u091C \u0921\u093E\u0909\u0928\u0932\u094B\
  \u0921 \u0915\u0930\u0928\u093E"
weight: 42
---

## How to: (कैसे करें:)
C# का इस्तेमाल करके, आप HttpClient क्लास के साथ बहुत आसानी से वेब पेज डाउनलोड कर सकते हैं।

```csharp
using System;
using System.Net.Http;
using System.Threading.Tasks;

class Program
{
    static async Task Main(string[] args)
    {
        HttpClient client = new HttpClient();
        try
        {
            string webpageContent = await client.GetStringAsync("http://www.example.com");
            Console.WriteLine(webpageContent);
        }
        catch (HttpRequestException e)
        {
            Console.WriteLine("\nException Caught!");
            Console.WriteLine("Message :{0} ", e.Message);
        }
    }
}
```

सैंपल आउटपुट वेब पेज के HTML कोड का होगा:

```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</html>
```

## Deep Dive (गहन अध्ययन)
पहले, WebRequest और WebClient का इस्तेमाल होता था। HttpClient नया और असिंक्रोनस तरीका प्रदान करता है जो परफॉर्मेंस और स्केलेबिलिटी में सुधार करता है। डाउनलोड किए गए वेब पेज को पार्स करने के लिए HtmlAgilityPack जैसे थर्ड-पार्टी लाइब्रेरीज़ का इस्तेमाल किया जाता है। वेब पेज डाउनलोड करने का कार्य कानूनी और एथिकल सीमाओं में होना चाहिए।

## See Also (और जाने)
- .NET में HttpClient के बारे में और पढ़ें: [Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient)
- HtmlAgilityPack का उपयोग कैसे करें: [HtmlAgilityPack](https://html-agility-pack.net/)
- वेब स्क्रैपिंग के कानूनी पहलू: [Web Scraping Legal Issues](https://benbernardblog.com/web-scraping-and-crawling-are-perfectly-legal-right/)
