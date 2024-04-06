---
date: 2024-01-26 04:28:58.934152-07:00
description: ''
lastmod: '2024-04-05T22:38:41.698331-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A XML\u306F90\u5E74\u4EE3\u5F8C\u534A\u304B\u3089\u5B58\
  \u5728\u3057\u3066\u304A\u308A\u3001\u30C6\u30AF\u30CE\u30ED\u30B8\u30FC\u306E\u5E74\
  \u9F62\u3067\u8A00\u3046\u3068\u304A\u3058\u3044\u3061\u3083\u3093\u3067\u3059\u3002\
  \u30C7\u30FC\u30BF\u306E\u79FB\u690D\u6027\u3068\u4EBA\u9593\u306E\u8AAD\u307F\u3084\
  \u3059\u3055\u306E\u305F\u3081\u306B\u8003\u6848\u3055\u308C\u307E\u3057\u305F\u3002\
  \u4ECA\u3067\u306F\u3001\u7279\u306BWeb\u306E\u6587\u8108\u3067\u3001\u3088\u308A\
  \u8EFD\u304F\u3001\u591A\u304F\u306E\u4EBA\u306B\u3068\u3063\u3066\u6271\u3044\u3084\
  \u3059\u3044JSON\u306A\u3069\u306E\u4EE3\u66FF\u624B\u6BB5\u304CXML\u306B\u8FEB\u3063\
  \u3066\u3044\u307E\u3059\u3002\u3057\u304B\u3057\u3001XML\u306F\u591A\u304F\u306E\
  \u30EC\u30AC\u30B7\u30FC\u30B7\u30B9\u30C6\u30E0\u3084\u7279\u5B9A\u306E\u901A\u4FE1\
  \u30D7\u30ED\u30C8\u30B3\u30EB\u3067\u307E\u3060\u5F37\u56FA\u306A\u5730\u4F4D\u3092\
  \u4FDD\u3063\u3066\u3044\u307E\u3059\u3002XML\u3092\u4F7F\u3046\u3068\u3001\u69CB\
  \u9020\u3092\u691C\u8A3C\u3059\u308B\u305F\u3081\u306E\u30B9\u30AD\u30FC\u30DE\u3068\
  \u30BF\u30B0\u306E\u885D\u7A81\u3092\u907F\u3051\u308B\u305F\u3081\u306E\u540D\u524D\
  \u7A7A\u9593\u304C\u5F97\u3089\u308C\u307E\u3059\u3002\u3053\u308C\u3089\u306F\u305D\
  \u306E\u30A8\u30F3\u30BF\u30FC\u30D7\u30E9\u30A4\u30BA\u6E96\u5099\u304C\u6574\u3063\
  \u305F\u6210\u719F\u5EA6\u3092\u7269\u8A9E\u3063\u3066\u3044\u307E\u3059\u3002 C#\u3067\
  \u306E`System.Xml.Linq`\u3068`System.Xml`\u540D\u524D\u7A7A\u9593\u306F\u3001XML\u3092\
  \u6271\u3046\u305F\u3081\u306E\u4E8C\u3064\u306E\u5927\u7832\u3067\u3059\u3002LINQ\
  \ to XML\uFF08`XDocument`\u3001`XElement`\uFF09\u306F\u3001\u3088\u308A\u73FE\u4EE3\
  \u7684\u3067\u30A8\u30EC\u30AC\u30F3\u30C8\u3067\u3059\u3002\u305D\u306E\u9B54\u6CD5\
  \u306F\u4F8B\u3067\u898B\u305F\u901A\u308A\u3067\u3059\u3002`XmlDocument`\u306F\u3001\
  DOM\uFF08Document Object Model\uFF09\u30A2\u30D7\u30ED\u30FC\u30C1\u3092\u63D0\u4F9B\
  \u3057\u307E\u3059\u3002\u5C11\u3057\u53E4\u98A8\u3067\u3059\u304C\u3001\u305D\u306E\
  \u529B\u3092\u4FE1\u3058\u308B\u4EBA\u3082\u3044\u307E\u3059\u3002"
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
