---
title:                "C#: एक वेब पृष्ठ डाउनलोड करना"
simple_title:         "एक वेब पृष्ठ डाउनलोड करना"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## क्यों
वेब पेज को डाउनलोड करने का कारण क्या है? आमतौर पर, एक उपयोगकर्ता इंटरनेट पर जाकर किसी वेब पेज का उपयोग करता है, लेकिन कभी-कभी किसी वेब पेज को अपने स्थानीय कंप्यूटर पर डाउनलोड करने की आवश्यकता होती है। जब आप एक वेब पेज को डाउनलोड करते हैं, तो आप उसे ऑफलाइन देख सकते हैं और उसमें संग्रहित जानकारी को बदल भी सकते हैं।

## कैसे करें
एक वेब पेज को C# में डाउनलोड करने के लिए, आपको आगे दिए गए कोड का उपयोग करना होगा:

```C#
using System;
using System.Net;
using System.IO;

class Program
{
    static void Main()
    {
        // URL of the web page to be downloaded
        string url = "https://www.example.com";
        
        // Create an instance of WebClient to download the page
        WebClient client = new WebClient();
        
        // Download the page and save it as a string
        string webPage = client.DownloadString(url);
        
        // Save the downloaded string to a local file
        File.WriteAllText("example.html", webPage);
        
        // Output a success message
        Console.WriteLine("Web page downloaded successfully.");
    }
}
```

इस कोड का उपयोग करके, आप एक वेब पेज को बहुत ही आसानी से अपने स्थानीय कंप्यूटर पर डाउनलोड कर सकते हैं। कोड चालू करने के बाद, एक फ़ाइल नाम "example.html" के साथ वेब पेज आपके कंप्यूटर पर संग्रहीत होगा।

## गहराई में खोजें
वेब पेज को डाउनलोड करने के बारे में अधिक जानने के लिए, आप वेब पेज के HTML कोड से भी खेल सकते हैं। आप उस डाउनलोड फ़ाइल को खोलकर, उसमें संग्रहीत हुए डेटा को देख सकते हैं और उसे अपनी इच्छा अनुसार संशोधित भी कर सकते हैं। आप पाठांकन उपकरण जैसे कि Notepad++ या Sublime Text का उपयोग करके वेब