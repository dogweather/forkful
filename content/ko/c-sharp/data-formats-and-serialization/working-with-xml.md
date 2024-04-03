---
date: 2024-01-26 04:29:37.874940-07:00
description: "XML(eXtensible Markup Language)\uC740 \uB370\uC774\uD130\uB97C \uC77D\
  \uAE30 \uC26C\uC6B4 \uD615\uC2DD\uC73C\uB85C \uAD6C\uC870\uD654\uD558\uB294 \uAC83\
  \uC5D0 \uAD00\uD55C \uAC83\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\
  \uC740 \uC124\uC815, \uC571 \uAC04 \uB370\uC774\uD130 \uAD50\uD658, \uADF8\uB9AC\
  \uACE0 SOAP\uC774\uB098 \uC6F9 API\uC640 \uAC19\uC740 \uC0AC\uC591\uC744 \uC694\uAD6C\
  \uD560 \uB54C XML\uC744 \uB2E4\uB8F9\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.270569-06:00'
model: gpt-4-0125-preview
summary: "XML(eXtensible Markup Language)\uC740 \uB370\uC774\uD130\uB97C \uC77D\uAE30\
  \ \uC26C\uC6B4 \uD615\uC2DD\uC73C\uB85C \uAD6C\uC870\uD654\uD558\uB294 \uAC83\uC5D0\
  \ \uAD00\uD55C \uAC83\uC785\uB2C8\uB2E4."
title: "XML \uB2E4\uB8E8\uAE30"
weight: 40
---

## 무엇인가 & 왜인가?
XML(eXtensible Markup Language)은 데이터를 읽기 쉬운 형식으로 구조화하는 것에 관한 것입니다. 프로그래머들은 설정, 앱 간 데이터 교환, 그리고 SOAP이나 웹 API와 같은 사양을 요구할 때 XML을 다룹니다.

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
