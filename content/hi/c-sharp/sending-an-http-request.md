---
title:                "एक http अनुरोध भेजना"
html_title:           "C#: एक http अनुरोध भेजना"
simple_title:         "एक http अनुरोध भेजना"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

HTTP अनुरोध भेजना एक प्रोग्रामर क्या होता है और वे इसे क्यों करते हैं को समझने के लिए हमें थोड़े से विवरण की आवश्यकता होती है। HTTP अनुरोध भेजना एक संपर्क बनाने का एक तरीका होता है जो एक कंप्यूटर एक्सचेंज इनफार्मेशन को दूरस्थ कंप्यूटर संसाधनों से या एक वेब सर्वर से लेता है।

## कैसे करें:

इस विशेष उदाहरण में, हम एक बसिक GET HTTP अनुरोध भेजना सीखेंगे। निम्नलिखित एम्पल कोड देखें:

```C#
using System;
using System.Net;

public class Program {
    public static void Main() {
        var url = "https://jsonplaceholder.typicode.com/posts/1"; // प्रतिस्थापित अपनी स्वयं की यूआरएल यहाँ
        var request = (HttpWebRequest)WebRequest.Create(url);
        request.Method = "GET";
        // यहां आप आवश्यकतानुसार अधिक फ़ील्ड्स जोड़ सकते हैं
        using (var response = (HttpWebResponse)request.GetResponse()) {
            if (response.StatusCode == HttpStatusCode.OK) {
                using (var dataStream = response.GetResponseStream()) {
                    var reader = new StreamReader(dataStream);
                    // डेटा को छांटें और प्रिंट करें
                    Console.WriteLine(reader.ReadToEnd());
                }
            }
        }
    }
}
```

उपरोक्त कोड का आउटपुट निम्नलिखित रूप में हो सकता है (दिया हुआ अनुरोध उदाहरण के लिए):

```
{
  "userId": 1,
  "id": 1,
  "title": "sunt aut facere repellat provident occaecati excepturi optio reprehenderit",
  "body": "quia et suscipit\nsuscipit recusandae consequuntur expedita et cum\nreprehenderit ...",
}
```

## गहराई में जाओ:

HTTP अनुरोध भेजने का एक प्राचीन और महत्वपूर्ण इतिहास है। यह विठरल रहा है क्योंकि यह वेब और एपीआई के रूप में नेटवर्क के अन्य इंटरएक्टिव रुख प्रदान करने में सक्षम है। कुछ अलग रूपों में भी उपलब्ध हैं, जैसे कि REST और SOAP, जो प्रोग्रामरों को अपने अनुरोधों को समेकित और सुरक्षित रूप से भेजने में मदद करते हैं। इसके अलावा, वेब अनुरोधों को भेजने के लिए कई लाइब्रेरी और फ्रेमवर्क भी हैं जो प्रोग्रामरों को संचित डेटा और सुरक्षा को निर्धारित तरीके से हैंडल करने में मदद करते हैं।

## और भी देखें:

- [HTTP क्या है](https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview)
- [C# में HTTP अनुरोध कैसे भेजें](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient?view=netcore-3.1)
- [REST vs SOAP](https://www.soapui.org/soap-vs-rest.html)