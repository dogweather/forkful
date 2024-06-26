---
date: 2024-01-26 04:29:37.874940-07:00
description: "\uBC29\uBC95: XML\uC740 90\uB144\uB300 \uD6C4\uBC18\uBD80\uD130 \uC874\
  \uC7AC\uD574\uC654\uC73C\uBA70, \uAE30\uC220 \uC5F0\uB300\uAE30\uB85C \uCE58\uBA74\
  \ \uD560\uC544\uBC84\uC9C0\uB098 \uB2E4\uB984\uC5C6\uC2B5\uB2C8\uB2E4. \uB370\uC774\
  \uD130\uC758 \uC774\uB3D9\uC131\uACFC \uC0AC\uB78C\uC774 \uC77D\uAE30 \uC26C\uC6C0\
  \uC744 \uC704\uD574 \uACE0\uC548\uB418\uC5C8\uC2B5\uB2C8\uB2E4. JSON\uACFC \uAC19\
  \uC740 \uB300\uCCB4\uC7AC\uAC00 \uC6F9 \uB9E5\uB77D\uC5D0\uC11C \uADF8 \uB4A4\uB97C\
  \ \uCAD3\uACE0 \uC788\uC73C\uBA70, \uB354 \uAC00\uBCCD\uACE0 \uB2E4\uB8E8\uAE30\
  \ \uC26C\uC6C0\uC744 \uC120\uD638\uD558\uB294 \uB9CE\uC740 \uC0AC\uB78C\uB4E4\uC5D0\
  \uAC8C \uC8FC\uBAA9\uBC1B\uACE0\u2026"
lastmod: '2024-04-05T21:53:56.989081-06:00'
model: gpt-4-0125-preview
summary: "XML\uC740 90\uB144\uB300 \uD6C4\uBC18\uBD80\uD130 \uC874\uC7AC\uD574\uC654\
  \uC73C\uBA70, \uAE30\uC220 \uC5F0\uB300\uAE30\uB85C \uCE58\uBA74 \uD560\uC544\uBC84\
  \uC9C0\uB098 \uB2E4\uB984\uC5C6\uC2B5\uB2C8\uB2E4."
title: "XML \uB2E4\uB8E8\uAE30"
weight: 40
---

## 방법:
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

        // 문자열을 XDocument로 파싱
        XDocument doc = XDocument.Parse(xmlString);

        // 새로운 책 추가
        doc.Element("bookstore").Add(
            new XElement("book",
                new XElement("title", "Learning XML", new XAttribute("lang", "en")),
                new XElement("price", 29.99)
            )
        );

        // 콘솔에 XML 작성
        Console.WriteLine(doc);

        // 문서 로드
        XmlDocument xmlDoc = new XmlDocument();
        xmlDoc.LoadXml(xmlString);

        // 모든 가격 검색
        XmlNodeList prices = xmlDoc.GetElementsByTagName("price");
        foreach (XmlNode price in prices)
        {
            Console.WriteLine(price.InnerText);
        }
     }
}

// 샘플 출력:
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

## 심층 분석
XML은 90년대 후반부터 존재해왔으며, 기술 연대기로 치면 할아버지나 다름없습니다. 데이터의 이동성과 사람이 읽기 쉬움을 위해 고안되었습니다. JSON과 같은 대체재가 웹 맥락에서 그 뒤를 쫓고 있으며, 더 가볍고 다루기 쉬움을 선호하는 많은 사람들에게 주목받고 있습니다. 하지만 XML은 여전히 수많은 레거시 시스템과 일부 통신 프로토콜에서 자신의 자리를 지키고 있습니다. XML을 사용하면 구조를 검증할 수 있는 스키마와 태그 충돌을 피할 수 있는 네임스페이스 등, 기업용으로 준비된 성숙함이 느껴지는 기능을 얻을 수 있습니다.

C#에서 `System.Xml.Linq`와 `System.Xml` 네임스페이스는 XML을 다루는 두 가지 큰 수단입니다. LINQ to XML(`XDocument`, `XElement`)은 더 현대적이고 우아합니다 - 예제에서 그 마법을 보셨을 겁니다. `XmlDocument`은 DOM(Document Object Model) 접근 방식을 제공합니다 - 조금 구식이지만, 일부 사람들은 그 힘을 믿고 있습니다.

## 참고 자료
- [MSDN – LINQ to XML 개요](https://docs.microsoft.com/dotnet/standard/linq/linq-xml-overview)
- [MSDN – XML 문서 객체 모델 (DOM)](https://docs.microsoft.com/dotnet/standard/data/xml/)
- [W3Schools – XML 배우기](https://www.w3schools.com/xml/)
- [XML 대 JSON](https://www.json.org/xml.html)
