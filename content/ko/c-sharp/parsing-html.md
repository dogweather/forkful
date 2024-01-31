---
title:                "HTML 파싱"
date:                  2024-01-20T15:30:30.637254-07:00
html_title:           "Arduino: HTML 파싱"
simple_title:         "HTML 파싱"

category:             "C#"
tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (무엇을 왜?)
HTML 파싱은 웹페이지의 마크업을 분석해서 데이터를 추출하는 과정입니다. 프로그래머들은 자동화된 방식으로 웹 콘텐츠를 읽고 처리하기 위해 이 작업을 합니다.

## How to: (방법)
C#에서 HTML을 파싱하려면 HtmlAgilityPack과 같은 라이브러리를 사용하는 것이 편합니다. 아래는 간단한 예시 코드입니다:

```C#
using HtmlAgilityPack;
using System;
using System.Linq;

class Program
{
    static void Main(string[] args)
    {
        HtmlWeb web = new HtmlWeb();
        HtmlDocument doc = web.Load("http://example.com");

        foreach (HtmlNode node in doc.DocumentNode.SelectNodes("//a[@href]"))
        {
            string hrefValue = node.GetAttributeValue("href", string.Empty);
            Console.WriteLine(hrefValue);
        }
    }
}
```
예시 출력:
```
/page1.html
/page2.html
/page3.html
```
이 코드는 웹페이지를 로드하고 모든 링크를 출력합니다.

## Deep Dive (심층 분석)
HTML 파싱은 웹 초기부터 필요했습니다. 상대적으로 쉬운 HTML은 파싱하기 적합한 언어입니다만, 종종 복잡한 구조를 가집니다. HtmlAgilityPack은 Microsoft .NET 환경에서 널리 사용되는 파서입니다. XPath 문법을 이용해 특정 노드를 쉽게 선택할 수 있습니다. 대안으로는 AngleSharp 등이 있으며, 각각의 성능과 기능이 다릅니다.

HTML 파싱에서 중요한 이슈는 HTML이 항상 잘 형성되지 않을 수 있다는 것입니다. 현실 세계의 HTML 문서는 종종 규칙을 따르지 않으므로 로버스트한 파서가 중요합니다. HtmlAgilityPack과 같은 도구는 이러한 문제를 해결하고자 잘못 형성된 HTML을 수정하거나 무시하는 기능을 제공합니다.

## See Also (참고할 자료)
- HtmlAgilityPack GitHub 페이지: https://github.com/zzzprojects/html-agility-pack
- AngleSharp GitHub 페이지: https://github.com/AngleSharp/AngleSharp
- C#에서 XPath를 사용한 HTML 파싱 방법: https://www.w3schools.com/xml/xpath_intro.asp
