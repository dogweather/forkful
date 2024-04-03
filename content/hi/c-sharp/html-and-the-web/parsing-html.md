---
date: 2024-01-20 15:30:46.117574-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u0906\u0907\
  \u090F C# \u092E\u0947\u0902 HTML parsing \u0915\u0930\u0928\u0947 \u0915\u0947\
  \ \u0932\u093F\u090F `HtmlAgilityPack` library \u0915\u093E \u0909\u092A\u092F\u094B\
  \u0917 \u0915\u0930\u0924\u0947 \u0939\u0948\u0902\u0964."
lastmod: '2024-03-13T22:44:52.327928-06:00'
model: unknown
summary: "\u0906\u0907\u090F C# \u092E\u0947\u0902 HTML parsing \u0915\u0930\u0928\
  \u0947 \u0915\u0947 \u0932\u093F\u090F `HtmlAgilityPack` library \u0915\u093E \u0909\
  \u092A\u092F\u094B\u0917 \u0915\u0930\u0924\u0947 \u0939\u0948\u0902\u0964."
title: "HTML \u092A\u093E\u0930\u094D\u0938 \u0915\u0930\u0928\u093E"
weight: 43
---

## How to: (कैसे करें:)
आइए C# में HTML parsing करने के लिए `HtmlAgilityPack` library का उपयोग करते हैं।

```C#
using HtmlAgilityPack;
using System;
using System.Net.Http;
using System.Threading.Tasks;

class Program
{
    static async Task Main(string[] args)
    {
        var url = "http://example.com";
        var httpClient = new HttpClient();
        var html = await httpClient.GetStringAsync(url);

        var htmlDoc = new HtmlDocument();
        htmlDoc.LoadHtml(html);

        var nodes = htmlDoc.DocumentNode.SelectNodes("//h1");
        
        foreach (var node in nodes)
        {
            Console.WriteLine(node.InnerText);
        }
    }
}
```
Sample output इस प्रकार हो सकता है:
```
Example Domain
```
यह कोड example.com से H1 tags के अंदर का text प्रिंट करता है।

## Deep Dive (गहराई से समझें)
HTML parsing की जरूरत पहली बार महसूस की गई जब internet पर बहुतायत से information मिलने लगी। पिछले समय में, जैसे-जैसे websites की संरचना जटिल होती गई, HTML parsing के method भी advanced होते गए।

`HtmlAgilityPack` .NET के लिए एक popular HTML parsing library है, जो कि robust है और broken HTML को भी handle कर सकती है। अन्य alternatives में `AngleSharp` जैसी libraries भी शामिल हैं जो modern web standards को support करती हैं।

Implementation में, parsers XHTML और HTML standards के अनुसार DOM (Document Object Model) तैयार करते हैं, जिससे developers के लिए specific nodes ढूंढना और manipulate करना आसान हो जाता है।

## See Also (और जानकारी के लिए)
- HtmlAgilityPack GitHub page: [HtmlAgilityPack](https://github.com/zzzprojects/html-agility-pack)
- AngleSharp GitHub page: [AngleSharp](https://github.com/AngleSharp/AngleSharp)
- W3C standards for HTML: [W3C HTML](https://www.w3.org/TR/html52/)
