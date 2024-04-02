---
date: 2024-01-20 18:02:00.492969-07:00
description: "HTTP \u0905\u0928\u0941\u0930\u094B\u0927\u094B\u0902 \u0915\u094B \u092C\
  \u0947\u0938\u093F\u0915 \u0911\u0925\u0947\u0902\u091F\u093F\u0915\u0947\u0936\u0928\
  \ \u0915\u0947 \u0938\u093E\u0925 \u092D\u0947\u091C\u0928\u093E \u0915\u093E \u092E\
  \u0924\u0932\u092C \u0939\u0948, \u092F\u0942\u091C\u093C\u0930 \u0928\u0947\u092E\
  \ \u0914\u0930 \u092A\u093E\u0938\u0935\u0930\u094D\u0921 \u0915\u0947 \u091C\u093C\
  \u0930\u093F\u090F \u0938\u0941\u0930\u0915\u094D\u0937\u093F\u0924 \u0924\u0930\
  \u0940\u0915\u0947 \u0938\u0947 \u0938\u0930\u094D\u0935\u0930 \u0938\u0947 \u0921\
  \u0947\u091F\u093E \u090F\u0915\u094D\u0938\u0947\u0938 \u0915\u0930\u0928\u093E\
  \u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930 \u0907\u0938\
  \u0915\u093E\u2026"
lastmod: '2024-03-13T22:44:52.331010-06:00'
model: gpt-4-1106-preview
summary: "HTTP \u0905\u0928\u0941\u0930\u094B\u0927\u094B\u0902 \u0915\u094B \u092C\
  \u0947\u0938\u093F\u0915 \u0911\u0925\u0947\u0902\u091F\u093F\u0915\u0947\u0936\u0928\
  \ \u0915\u0947 \u0938\u093E\u0925 \u092D\u0947\u091C\u0928\u093E \u0915\u093E \u092E\
  \u0924\u0932\u092C \u0939\u0948, \u092F\u0942\u091C\u093C\u0930 \u0928\u0947\u092E\
  \ \u0914\u0930 \u092A\u093E\u0938\u0935\u0930\u094D\u0921 \u0915\u0947 \u091C\u093C\
  \u0930\u093F\u090F \u0938\u0941\u0930\u0915\u094D\u0937\u093F\u0924 \u0924\u0930\
  \u0940\u0915\u0947 \u0938\u0947 \u0938\u0930\u094D\u0935\u0930 \u0938\u0947 \u0921\
  \u0947\u091F\u093E \u090F\u0915\u094D\u0938\u0947\u0938 \u0915\u0930\u0928\u093E\
  \u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930 \u0907\u0938\
  \u0915\u093E\u2026"
title: "\u092C\u0947\u0938\u093F\u0915 \u092A\u094D\u0930\u092E\u093E\u0923\u0940\u0915\
  \u0930\u0923 \u0915\u0947 \u0938\u093E\u0925 HTTP \u0905\u0928\u0941\u0930\u094B\
  \u0927 \u092D\u0947\u091C\u0928\u093E"
weight: 45
---

## क्या और क्यों? (What & Why?)
HTTP अनुरोधों को बेसिक ऑथेंटिकेशन के साथ भेजना का मतलब है, यूज़र नेम और पासवर्ड के ज़रिए सुरक्षित तरीके से सर्वर से डेटा एक्सेस करना। प्रोग्रामर इसका उपयोग तब करते हैं जब उन्हें किसी एपीआई या वेब सेवा से विशेष डेटा प्राप्त करना होता है जो सीमित पहुँच वाला हो।

## कैसे करें: (How to:)
```C#
using System;
using System.Net.Http;
using System.Text;
using System.Threading.Tasks;

class BasicAuthExample
{
    static async Task Main()
    {
        using var client = new HttpClient();
        
        // बेसिक ऑथेंटिकेशन क्रेडेंशियल्स को बेस64 में एनकोड करें
        var credentials = Convert.ToBase64String(Encoding.ASCII.GetBytes("username:password"));
        client.DefaultRequestHeaders.Authorization = new System.Net.Http.Headers.AuthenticationHeaderValue("Basic", credentials);

        // HTTP GET अनुरोध भेजें
        var result = await client.GetAsync("https://api.example.com/data");
        var content = await result.Content.ReadAsStringAsync();
        Console.WriteLine(content);
    }
}
```
उपरोक्त कोड सरल HTTP GET अनुरोध भेजता है जिसमें बेसिक ऑथेंटिकेशन है। यानी, जब आप इसे चलाते हैं, आपको सर्वर से प्रतिक्रिया में डेटा मिलेगा, जैसे कि जेसन या एचटीएमएल।

## गहराई से समझ (Deep Dive)
बेसिक ऑथेंटिकेशन HTTP प्रोटोकॉल में सबसे पुराने तरीकों में से एक है, लेकिन इसमें सुरक्षा जोखिम हो सकते हैं क्योंकि क्रेडेंशियल्स को बेस64 एनकोडिंग के ज़रिए सादे पाठ में भेजा जाता है। इसलिए, हमेशा HTTPS का उपयोग करें।

विकल्प में, OAuth या JWT (JSON Web Tokens) जैसी अधिक सुरक्षित पद्धतियां हैं, जो बेहतर सुरक्षा प्रदान करते हैं।

बेसिक ऑथेंटिकेशन कार्यान्वित करते समय, `HttpClient` के डिफॉल्ट रिक्वेस्ट हेडर का उपयोग करें ताकि सभी अनुरोधों में ऑथेंटिकेशन हेडर शामिल हो। `Convert.ToBase64String` और `Encoding.ASCII.GetBytes` का उपयोग करके यूजर नेम और पासवर्ड को एनकोड किया जा सकता है।

## यह भी देखें (See Also)
- [HttpClient Class Documentation](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient)
- [Basic Authentication on MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#basic_authentication_scheme)
- [Understanding Basic Authentication](https://www.ietf.org/rfc/rfc2617.txt)
