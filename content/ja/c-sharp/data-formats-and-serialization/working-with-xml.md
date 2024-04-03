---
date: 2024-01-26 04:28:58.934152-07:00
description: "XML\uFF08eXtensible Markup Language\uFF09\u306F\u3001\u30C7\u30FC\u30BF\
  \u3092\u8AAD\u307F\u3084\u3059\u3044\u5F62\u5F0F\u3067\u69CB\u9020\u5316\u3059\u308B\
  \u3053\u3068\u306B\u3064\u3044\u3066\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u306F\u8A2D\u5B9A\u3001\u30A2\u30D7\u30EA\u9593\u306E\u30C7\u30FC\u30BF\u4EA4\
  \u63DB\u3001\u4ED5\u69D8\u304C\u8981\u6C42\u3059\u308B\u5834\u5408\uFF08SOAP\u3084\
  Web API\u3092\u8003\u3048\u3066\u307F\u3066\u304F\u3060\u3055\u3044\uFF09\u3067\
  XML\u3092\u6271\u3044\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:42.156921-06:00'
model: gpt-4-0125-preview
summary: "XML\uFF08eXtensible Markup Language\uFF09\u306F\u3001\u30C7\u30FC\u30BF\u3092\
  \u8AAD\u307F\u3084\u3059\u3044\u5F62\u5F0F\u3067\u69CB\u9020\u5316\u3059\u308B\u3053\
  \u3068\u306B\u3064\u3044\u3066\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\
  \u306F\u8A2D\u5B9A\u3001\u30A2\u30D7\u30EA\u9593\u306E\u30C7\u30FC\u30BF\u4EA4\u63DB\
  \u3001\u4ED5\u69D8\u304C\u8981\u6C42\u3059\u308B\u5834\u5408\uFF08SOAP\u3084Web\
  \ API\u3092\u8003\u3048\u3066\u307F\u3066\u304F\u3060\u3055\u3044\uFF09\u3067XML\u3092\
  \u6271\u3044\u307E\u3059\u3002."
title: "XML\u306E\u6271\u3044\u65B9"
weight: 40
---

## 方法：
```C#
using System;
using System.Xml;
using System.Xml.Linq;

class Program
{
     static void Main()
     {
        var xmlString = @"<bookstore>
                            <book>
                              <title lang=""en"">Head First C#</title>
                              <price>39.99</price>
                            </book>
                          </bookstore>";

        // 文字列をXDocumentに解析する
        XDocument doc = XDocument.Parse(xmlString);

        // 新しい本を追加
        doc.Element("bookstore").Add(
            new XElement("book",
                new XElement("title", "Learning XML", new XAttribute("lang", "en")),
                new XElement("price", 29.99)
            )
        );

        // XMLをコンソールに出力
        Console.WriteLine(doc);

        // ドキュメントをロード
        XmlDocument xmlDoc = new XmlDocument();
        xmlDoc.LoadXml(xmlString);

        // すべての価格を取得
        XmlNodeList prices = xmlDoc.GetElementsByTagName("price");
        foreach (XmlNode price in prices)
        {
            Console.WriteLine(price.InnerText);
        }
     }
}

// サンプル出力：
// <bookstore>
//  <book>
//    <title lang="en">Head First C#</title>
//    <price>39.99</price>
//  </book>
//  <book>
//    <title lang="en">Learning XML</title>
//    <price>29.99</price>
//  </book>
// </bookstore>
// 39.99
// 29.99
```

## 詳細解説
XMLは90年代後半から存在しており、テクノロジーの年齢で言うとおじいちゃんです。データの移植性と人間の読みやすさのために考案されました。今では、特にWebの文脈で、より軽く、多くの人にとって扱いやすいJSONなどの代替手段がXMLに迫っています。しかし、XMLは多くのレガシーシステムや特定の通信プロトコルでまだ強固な地位を保っています。XMLを使うと、構造を検証するためのスキーマとタグの衝突を避けるための名前空間が得られます。これらはそのエンタープライズ準備が整った成熟度を物語っています。

C#での`System.Xml.Linq`と`System.Xml`名前空間は、XMLを扱うための二つの大砲です。LINQ to XML（`XDocument`、`XElement`）は、より現代的でエレガントです。その魔法は例で見た通りです。`XmlDocument`は、DOM（Document Object Model）アプローチを提供します。少し古風ですが、その力を信じる人もいます。

## 参照
- [MSDN – LINQ to XML 概要](https://docs.microsoft.com/dotnet/standard/linq/linq-xml-overview)
- [MSDN – XML ドキュメント オブジェクト モデル (DOM)](https://docs.microsoft.com/dotnet/standard/data/xml/)
- [W3Schools – XML 学習](https://www.w3schools.com/xml/)
- [XML vs. JSON](https://www.json.org/xml.html)
