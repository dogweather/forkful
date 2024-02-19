---
aliases:
- /hi/c-sharp/downloading-a-web-page/
date: 2024-01-20 17:43:40.134412-07:00
description: "\u0935\u0947\u092C \u092A\u0947\u091C \u0921\u093E\u0909\u0928\u0932\
  \u094B\u0921 \u0915\u0930\u0928\u093E \u092F\u093E\u0928\u0940 \u0907\u0902\u091F\
  \u0930\u0928\u0947\u091F \u0938\u0947 \u0938\u093E\u092E\u0917\u094D\u0930\u0940\
  \ \u092A\u094D\u0930\u093E\u092A\u094D\u0924 \u0915\u0930\u0928\u093E\u0964 \u092A\
  \u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u0921\u0947\u091F\
  \u093E \u090F\u0928\u093E\u0932\u093F\u0938\u093F\u0938, \u0938\u094D\u0915\u094D\
  \u0930\u0948\u092A\u093F\u0902\u0917 \u092F\u093E \u0911\u092B\u093C\u0932\u093E\
  \u0907\u0928 \u090F\u0915\u094D\u0938\u0947\u0938 \u0915\u0947 \u0932\u093F\u090F\
  \ \u0907\u0938\u0947 \u0915\u0930\u0924\u0947 \u0939\u0948\u0902\u0964"
lastmod: 2024-02-18 23:09:03.343835
model: gpt-4-1106-preview
summary: "\u0935\u0947\u092C \u092A\u0947\u091C \u0921\u093E\u0909\u0928\u0932\u094B\
  \u0921 \u0915\u0930\u0928\u093E \u092F\u093E\u0928\u0940 \u0907\u0902\u091F\u0930\
  \u0928\u0947\u091F \u0938\u0947 \u0938\u093E\u092E\u0917\u094D\u0930\u0940 \u092A\
  \u094D\u0930\u093E\u092A\u094D\u0924 \u0915\u0930\u0928\u093E\u0964 \u092A\u094D\
  \u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u0921\u0947\u091F\u093E\
  \ \u090F\u0928\u093E\u0932\u093F\u0938\u093F\u0938, \u0938\u094D\u0915\u094D\u0930\
  \u0948\u092A\u093F\u0902\u0917 \u092F\u093E \u0911\u092B\u093C\u0932\u093E\u0907\
  \u0928 \u090F\u0915\u094D\u0938\u0947\u0938 \u0915\u0947 \u0932\u093F\u090F \u0907\
  \u0938\u0947 \u0915\u0930\u0924\u0947 \u0939\u0948\u0902\u0964"
title: "\u0935\u0947\u092C \u092A\u0947\u091C \u0921\u093E\u0909\u0928\u0932\u094B\
  \u0921 \u0915\u0930\u0928\u093E"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
वेब पेज डाउनलोड करना यानी इंटरनेट से सामग्री प्राप्त करना। प्रोग्रामर्स डेटा एनालिसिस, स्क्रैपिंग या ऑफ़लाइन एक्सेस के लिए इसे करते हैं।

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
