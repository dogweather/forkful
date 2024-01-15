---
title:                "HTML का अनुवाद"
html_title:           "C#: HTML का अनुवाद"
simple_title:         "HTML का अनुवाद"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/parsing-html.md"
---

{{< edit_this_page >}}

## क्यों

जब हम वेबसाइटों को खोजते हैं, हमें अक्सर वेब पेजों से विभिन्न जानकारी और डेटा को प्राप्त करने की आवश्यकता पड़ती है। HTML वास्तव में वेब पेजों का रूप बनाता है और हम इस भाषा को उपयोग करके वेब पेजों से महत्वपूर्ण जानकारी को निकाल सकते हैं। इसलिए, HTML पार्सिंग करना हमारे लिए काफी उपयोगी हो सकता है।

## कैसे करें

```
C# using System;
using System.Net;
using System.IO;
using HtmlAgilityPack;

string url = "https://www.example.com";

// वेब पेज से डेटा प्राप्त करने के लिए रिक्वेस्ट भेजें
HttpWebRequest request = (HttpWebRequest)WebRequest.Create(url);
HttpWebResponse response = (HttpWebResponse)request.GetResponse();

// स्ट्रीम रीडर से रिस्पॉन्स डेटा पढ़ें
Stream dataStream = response.GetResponseStream();
StreamReader reader = new StreamReader(dataStream);
string responseFromServer = reader.ReadToEnd();

// HtmlDocument ऑब्जेक्ट बनाएं और उसे वेब पेज डेटा से लोड करें
HtmlDocument document = new HtmlDocument();
document.LoadHtml(responseFromServer);

// XPath का उपयोग करके चाहे गए डाटा को निकालें
string xpath = "//h1";
HtmlNodeCollection nodes = document.DocumentNode.SelectNodes(xpath);

// निकाले गए डाटा को प्रिंट करें
foreach (HtmlNode node in nodes)
{
    Console.WriteLine(node.InnerText);
}
```

आपको ऊपर दिए गए कोड से समझ आ गया होगा कि हम कैसे एक वेब पेज से डेटा को प्राप्त कर सकते हैं और XPath का उपयोग करके जैसी भी जानकारी हमें चाहिए, उसे वेब पेज से निकाल सकते हैं। इसके अलावा, हम HtmlAgilityPack नामक एक उपयोगी लाइब्रेरी भी उपयोग कर सकते हैं जो कि हमारे लिए पार्सिंग प्रक्रिया को और सरल बना देती है।

## गहराई में

HTML पार्सिंग करने के लिए अनेक तरीके हैं जो कि खासकर सभी के लिए फायदेमंद हो सकते ह