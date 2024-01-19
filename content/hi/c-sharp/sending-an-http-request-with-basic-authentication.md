---
title:                "बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
html_title:           "C#: बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
simple_title:         "बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
HTTP से बुनियादी प्रमाणीकरण के साथ अनुरोध भेजना आपके कोड को सर्वर से डेटा प्राप्त करने में सहायता करता है जिसे आपका कोड प्रक्रिया कर सकता है। इसका इस्तेमाल कांगोया (Confidentiality) और पहचान के सत्ता (Identity Assertion) के लिए किया जाता है।

## कैसे करें:
यहाँ एक साधारण सी# कोड है जो HTTP अनुरोध के साथ बुनियादी प्रमाणीकरण भेजता है:

```C#
using System;
using System.Net;
using System.Text;

class Program
{
    static void Main()
    {
        string url = "http://example.com";
        string username = "user";
        string password = "password";

        WebClient wc = new WebClient();

        string credentials = Convert.ToBase64String(Encoding.ASCII.GetBytes(username + ":" + password));
        wc.Headers[HttpRequestHeader.Authorization] = string.Format("Basic {0}", credentials);

        string result = wc.DownloadString(url);
        Console.WriteLine(result);
    }
}
```
इस कोड का आउटपुट सर्वर से प्राप्त डेटा होगा जैसे उसे एक वेब ब्राउज़र में दर्ज किया जाता है । 

## गहरा डाइव:
(1) ऐतिहासिक संदर्भ: बुनियादी प्रमाणीकरण का उदाहरण इंटरनेट सुरक्षा की शुरुआती मानकों में से एक है । 
(2) वैकल्पिक: आज के समय में, OAuth और Bearer Token जैसे अधिक सुरक्षित विधियों का इस्तेमाल भी किया जा सकता है। 
(3) क्रियान्वयन विवरण: हमारे कोड में, हम उपयोगकर्ता नाम और पासवर्ड को एक संयुक्त स्ट्रिंग में बदलते हैं , फिर उसे Base64 में कोड करते हैं, और अंत में हम इसे `Authorization` हैडर के रूप में जोड़ते हैं।

## और देखें:
HTTP प्रमाणीकरण के बारे में और जानने के लिए, निम्नलिखित लिंक पर क्लिक करें:
1. [HTTP Authentication (MDN Web Docs)](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
2. [The HTTP Authorization request header (MDN Web Docs)](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Authorization)
3. [Basic access authentication(Basic access authentication - Wikipedia)](https://en.wikipedia.org/wiki/Basic_access_authentication)