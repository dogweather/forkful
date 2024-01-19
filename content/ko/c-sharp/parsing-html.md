---
title:                "HTML 파싱"
html_title:           "Arduino: HTML 파싱"
simple_title:         "HTML 파싱"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/parsing-html.md"
---

{{< edit_this_page >}}

# HTML Parsing 이란 무엇이고 왜 필요한가?

## 무엇과 왜?
HTML Parsing은 HTML 문서나 문자열의 구조를 파악하고 오브젝트나 데이터로 변환하는 과정을 말합니다. 프로그래머들이 이를 하는 이유는 웹페이지의 데이터를 추출하거나 조작하려는 경우가 대부분입니다.

## 어떻게 하는가?
C#에서 HTML Parsing을 하려면 HtmlAgilityPack 라이브러리를 사용하면 됩니다. 아래는 간단한 예시입니다.

```C#
using HtmlAgilityPack;

var html = @"<html>
                 <head></head>
                 <body>
                     <h1>Welcome to HTML Parsing</h1>
                 </body>
             </html>";

var htmlDoc = new HtmlDocument();
htmlDoc.LoadHtml(html);

var node = htmlDoc.DocumentNode.SelectSingleNode("//h1");

Console.WriteLine(node.InnerHtml);
```
이 코드를 실행하면, 콘솔에는 "Welcome to HTML Parsing"이 출력됩니다.

## 깊은 이해
HTML Parsing은 웹 크롤링(데이터 스크래핑)의 주요 과정 중 하나로, 웹사이트의 정보를 수집하기 위해 개발된 기술입니다. 이 기술은 CSSOM, SAX, DOM 등 다양한 알고리즘을 기반으로 합니다.

HtmlAgilityPack 외에도 AngleSharp 같은 C# 라이브러리를 사용해 HTML을 파싱할 수 있습니다. 이 라이브러리를 사용하면 HTML 문서의 DOM을 쉽게 탐색하고 조작할 수 있습니다.

HTML Parsing 과정은 HTML 문서를 UTF-8 문자열로 로드하고, 이를 나무 구조의 노드로 변환하는 것으로 시작합니다. 이후 SelectSingleNode 또는 SelectNodes 메서드를 사용해 특정 요소를 검색하고 추출할 수 있습니다.

## 참고 자료
- HTMLAgilityPack 사용법: https://html-agility-pack.net/
- AngleSharp 라이브러리: https://anglesharp.github.io/
- 크롤링에 대한 깊은 이해: https://chaosweb.tistory.com/73
- 웹 스크래핑에 대한 기본 지식: https://realpython.com/beautiful-soup-web-scraper-python/