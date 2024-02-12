---
title:                "XMLの扱い方"
aliases:
- /ja/c-sharp/working-with-xml.md
date:                  2024-01-26T04:28:58.934152-07:00
model:                 gpt-4-0125-preview
simple_title:         "XMLの扱い方"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/working-with-xml.md"
---

{{< edit_this_page >}}

## 何となぜ？
XML（eXtensible Markup Language）は、データを読みやすい形式で構造化することについてです。プログラマーは設定、アプリ間のデータ交換、仕様が要求する場合（SOAPやWeb APIを考えてみてください）でXMLを扱います。

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
