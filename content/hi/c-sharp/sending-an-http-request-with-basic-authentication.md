---
title:                "बेसिक प्रमाणीकरण के साथ एचटीटीपी अनुरोध भेजें"
html_title:           "C#: बेसिक प्रमाणीकरण के साथ एचटीटीपी अनुरोध भेजें"
simple_title:         "बेसिक प्रमाणीकरण के साथ एचटीटीपी अनुरोध भेजें"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

HTTP के साथ बेसिक प्रमाणीकरण के साथ एक अनुरोध भेजना वे कार्य हैं जो प्रोग्रामर स्वयं बनाते हैं। यह उन्हें अन्य वेब सर्विसेज के साथ संवाद करने की अनुमति देता है अपने अनुप्रयोगों को।

## कैसे करें:

```C#
using System;
using System.Net;
using System.IO;

class Program
{
    static void Main(string[] args)
    {
        //यहाँ प्रयुक्त URL को बदल सकते हैं
        string url = "https://www.example.com";

        //एक नया HttpWebRequest बनाएं
        HttpWebRequest request = (HttpWebRequest)WebRequest.Create(url);

        //बेसिक प्रमाणीकरण चालू करें
        NetworkCredential credentials = new NetworkCredential("username", "password");
        request.Credentials = credentials;

        //उससे जुड़े अनुरोध भेजें
        HttpWebResponse response = (HttpWebResponse)request.GetResponse();

        //जवाब पढ़ें
        Stream dataStream = response.GetResponseStream();
        StreamReader reader = new StreamReader(dataStream);
        string responseFromServer = reader.ReadToEnd();

        //जवाब दिखाए
        Console.WriteLine(responseFromServer);
        Console.ReadLine();

        //जवाब से जुड़े संसाधन बंद करें
        reader.Close();
        response.Close();
    }
}
```

## गहराई में जाओ:

बेसिक प्रमाणीकरण HTTP अनुरोध से अधिक बेहतर सुरक्षा प्रदान करने के लिए विकसित किया गया था। इससे अन्य प्रकार के प्रमाणीकरण की तुलना में यह अत्यधिक सरल है। इसके अलावा, आप अपने अनुप्रयोगों को प्रमाणित उपयोगकर्ता के साथ बिना सर्वर साइड कोड तक पहुंच कर सुरक्षित रख सकते हैं। इसके साथ साथ, आप सरलता के साथ साइटों को प्रमाणित करने में भी सक्षम होंगे।

## अन्य लिंक:

- [बेसिक प्रमाणीकरण के साथ HTTP अनुरोध भेजना (Microsoft Docs)](https://docs.microsoft.com/en-us/dotnet/framework/network-programming/how-to-send-an-http-request-with-basic-authentication)
- [C# गाइड (Microsoft Docs)](https://docs.microsoft.com/en-us/dotnet/csharp/)
- [सीखने के लिए C# कोड (CodeCademy)](https://www.codecademy.com/learn/learn-c-sharp)