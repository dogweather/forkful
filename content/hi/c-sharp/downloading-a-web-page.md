---
title:                "वेब पेज डाउनलोड करना"
aliases:
- hi/c-sharp/downloading-a-web-page.md
date:                  2024-01-20T17:43:40.134412-07:00
model:                 gpt-4-1106-preview
simple_title:         "वेब पेज डाउनलोड करना"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/downloading-a-web-page.md"
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
