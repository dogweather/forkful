---
title:                "HTML पार्स करना"
aliases:
- /hi/c-sharp/parsing-html/
date:                  2024-01-20T15:30:46.117574-07:00
simple_title:         "HTML पार्स करना"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
HTML parsing का मतलब है HTML documents से data extract करना। Programmers इसका इस्तेमाल web pages से information निकालने के लिए करते हैं, जैसे कि web scraping, content analysis, या data migration.

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
