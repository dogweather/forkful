---
title:                "HTTP अनुरोध भेजना"
aliases: - /hi/c-sharp/sending-an-http-request.md
date:                  2024-01-20T18:00:14.559568-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTP अनुरोध भेजना"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
HTTP अनुरोध भेजना वेबसर्वर से जानकारी मांगने की प्रक्रिया है। प्रोग्रामर डेटा पाने, अपडेट करने, पोस्ट करने या डिलीट करने के लिए यह करते हैं। संक्षेप में, यह इंटरनेट पर संवाद स्थापित करने का एक तरीका है।

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
