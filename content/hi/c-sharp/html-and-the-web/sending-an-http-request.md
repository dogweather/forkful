---
date: 2024-01-20 18:00:14.559568-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) HTTP \u0905\
  \u0928\u0941\u0930\u094B\u0927 \u092D\u0947\u091C\u0928\u093E 1990 \u0915\u0947\
  \ \u0926\u0936\u0915 \u0938\u0947 \u0935\u0947\u092C \u0915\u093E \u090F\u0915 \u0939\
  \u093F\u0938\u094D\u0938\u093E \u0930\u0939\u093E \u0939\u0948\u0964 `HttpClient`\
  \ C# \u092E\u0947\u0902 \u0928\u092F\u093E \u0928\u0939\u0940\u0902 \u0939\u0948\
  , \u0932\u0947\u0915\u093F\u0928 \u092F\u0939 \u0905\u0927\u093F\u0915 \u0915\u0941\
  \u0936\u0932 \u0914\u0930 \u0906\u0938\u093E\u0928\u0940 \u0938\u0947 \u0909\u092A\
  \u092F\u094B\u0917 \u0915\u093F\u092F\u093E\u2026"
lastmod: '2024-04-05T22:51:07.015928-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) HTTP \u0905\u0928\u0941\
  \u0930\u094B\u0927 \u092D\u0947\u091C\u0928\u093E 1990 \u0915\u0947 \u0926\u0936\
  \u0915 \u0938\u0947 \u0935\u0947\u092C \u0915\u093E \u090F\u0915 \u0939\u093F\u0938\
  \u094D\u0938\u093E \u0930\u0939\u093E \u0939\u0948\u0964 `HttpClient` C# \u092E\u0947\
  \u0902 \u0928\u092F\u093E \u0928\u0939\u0940\u0902 \u0939\u0948, \u0932\u0947\u0915\
  \u093F\u0928 \u092F\u0939 \u0905\u0927\u093F\u0915 \u0915\u0941\u0936\u0932 \u0914\
  \u0930 \u0906\u0938\u093E\u0928\u0940 \u0938\u0947 \u0909\u092A\u092F\u094B\u0917\
  \ \u0915\u093F\u092F\u093E \u091C\u093E\u0928\u0947 \u0935\u093E\u0932\u093E \u0924\
  \u0930\u0940\u0915\u093E \u0939\u0948 \u091C\u093F\u0938\u0915\u0940 \u0936\u0941\
  \u0930\u0941\u0906\u0924 .NET framework 4.5 \u092E\u0947\u0902 \u0939\u0941\u0908\
  \ \u0925\u0940\u0964 \u0907\u0938\u0938\u0947 \u092A\u0939\u0932\u0947 `WebClient`\
  \ \u0914\u0930 `HttpWebRequest` \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0939\
  \u094B\u0924\u093E \u0925\u093E\u0964 `HttpClient` \u0915\u0941\u091B \u092B\u093E\
  \u092F\u0926\u094B\u0902 \u0915\u0947 \u0938\u093E\u0925 \u0906\u0924\u093E \u0939\
  \u0948, \u091C\u0948\u0938\u0947 \u0905\u0938\u093F\u0902\u0915\u094D\u0930\u094B\
  \u0928\u0938 \u0911\u092A\u0930\u0947\u0936\u0928, \u0906\u0938\u093E\u0928\u0940\
  \ \u0938\u0947 \u0915\u0928\u094D\u092B\u093F\u0917\u0930 \u0939\u094B\u0928\u0947\
  \ \u0935\u093E\u0932\u0947 \u0939\u0947\u0921\u0930\u094D\u0938, \u0914\u0930 \u0930\
  \u093F\u0938\u094D\u092A\u093E\u0902\u0938 \u0915\u0948\u0936\u093F\u0902\u0917\u0964\
  \ \u0905\u0932\u0917 \u0905\u0932\u0917 HTTP \u092E\u0947\u0925\u0921\u094D\u0938\
  \ (GET, POST, PUT, DELETE) \u0905\u0932\u0917 \u0905\u0932\u0917 \u0909\u0926\u094D\
  \u0926\u0947\u0936\u094D\u092F \u0915\u0947 \u0932\u093F\u090F \u0939\u094B\u0924\
  \u0947 \u0939\u0948\u0902 - GET \u0921\u0947\u091F\u093E \u092A\u093E\u0928\u0947\
  \ \u0915\u0947 \u0932\u093F\u090F, POST \u0928\u090F \u0921\u0947\u091F\u093E \u092D\
  \u0947\u091C\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F, PUT \u0921\u0947\u091F\
  \u093E \u0905\u092A\u0921\u0947\u091F \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\
  \u093F\u090F, \u0914\u0930 DELETE \u0921\u0947\u091F\u093E \u092E\u093F\u091F\u093E\
  \u0928\u0947 \u0915\u0947 \u0932\u093F\u090F\u0964 `HttpClient` \u092F\u0947 \u0938\
  \u092D\u0940 \u0938\u092E\u0930\u094D\u0925\u0928 \u0915\u0930\u0924\u093E \u0939\
  \u0948\u0964."
title: "HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092D\u0947\u091C\u0928\u093E"
weight: 44
---

## How to: (कैसे करें:)
```C#
// HttpClient का उपयोग करके एक GET अनुरोध भेजें
using System;
using System.Net.Http;
using System.Threading.Tasks;

class Program
{
    static async Task Main()
    {
        // HttpClient इंस्टेंस बनाएं
        using (var client = new HttpClient())
        {
            // GET अनुरोध भेजें
            HttpResponseMessage response = await client.GetAsync("http://example.com/api/data");

            // स्थिति कोड चेक करें
            if (response.IsSuccessStatusCode)
            {
                // रिस्पांस बॉडी पढ़ें
                string responseBody = await response.Content.ReadAsStringAsync();
                Console.WriteLine(responseBody);
            }
            else
            {
                Console.WriteLine($"Error: {response.StatusCode}");
            }
        }
    }
}
```

Sample Output:
```
{"name":"John Doe","email":"john@example.com"}
```
ऊपर कोड में, हमने `HttpClient` का उपयोग करके एक साधारण GET अनुरोध भेजा और JSON प्रतिक्रिया पाई।

## Deep Dive (गहराई से जानकारी)
HTTP अनुरोध भेजना 1990 के दशक से वेब का एक हिस्सा रहा है। `HttpClient` C# में नया नहीं है, लेकिन यह अधिक कुशल और आसानी से उपयोग किया जाने वाला तरीका है जिसकी शुरुआत .NET framework 4.5 में हुई थी। इससे पहले `WebClient` और `HttpWebRequest` का उपयोग होता था। `HttpClient` कुछ फायदों के साथ आता है, जैसे असिंक्रोनस ऑपरेशन, आसानी से कन्फिगर होने वाले हेडर्स, और रिस्पांस कैशिंग। 

अलग अलग HTTP मेथड्स (GET, POST, PUT, DELETE) अलग अलग उद्देश्य के लिए होते हैं - GET डेटा पाने के लिए, POST नए डेटा भेजने के लिए, PUT डेटा अपडेट करने के लिए, और DELETE डेटा मिटाने के लिए। `HttpClient` ये सभी समर्थन करता है।

## See Also (और देखें)
- [HttpClient Class Documentation - Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient)
- [HTTP Request Methods - w3schools](https://www.w3schools.com/tags/ref_httpmethods.asp)
- [.NET Asynchronous Programming Model - Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/concepts/async/)
- [Introduction to REST and .NET Core - Microsoft Docs](https://docs.microsoft.com/en-us/aspnet/core/web-api/?view=aspnetcore-3.1)
