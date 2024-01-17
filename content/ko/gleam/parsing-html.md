---
title:                "HTML 파싱"
html_title:           "Gleam: HTML 파싱"
simple_title:         "HTML 파싱"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/parsing-html.md"
---

{{< edit_this_page >}}

# 지금 Gleam으로 HTML 파싱하기

## 무엇이며, 그래서 왜?
HTML 파싱은 웹 서버에서 받은 HTML 코드를 읽고 데이터를 추출하는 것을 말합니다. 프로그래머들은 파싱을 통해 웹 사이트의 정보를 가져와서 더 나은 사용자 경험을 제공하거나 데이터를 분석하는 등 다양한 목적을 달성할 수 있습니다.

## 어떻게:
```Gleam
// HTML 코드를 문자열로 저장한다.
let html = "<div class='title'>Gleam 파싱</div>"

// Gleam의 HTML 파싱 라이브러리를 불러온다.
import gleam/html/parser

// HTML 코드를 파싱하여 원하는 태그의 내용을 추출한다.
let title = html
            |> parser.parse()
            |> parser.find_one([ "div", class("title") ])
            |> parser.inner_text()

// 결과를 출력한다.
IO.println(title) // Gleam 파싱

```

## 더 자세히:
HTML 파싱은 웹 개발에서 필수적인 기술이며, 다양한 라이브러리와 도구들이 이용되고 있습니다. 예를 들어, 파이썬의 BeautifulSoup나 자바스크립트의 Cheerio는 HTML 파싱을 위해 널리 알려진 라이브러리입니다. Gleam 또한 간편한 문법과 강력한 타입 시스템을 통해 HTML 파싱을 손쉽게 할 수 있도록 도와줍니다.

## 참고:
- [Gleam 공식 홈페이지](https://gleam.run/)
- [Gleam의 HTML 파싱 라이브러리: gleam/html-parser](https://github.com/gleam-lang/html-parser)
- [파이썬 BeautifulSoup](https://www.crummy.com/software/BeautifulSoup/)
- [자바스크립트 Cheerio](https://cheerio.js.org/)