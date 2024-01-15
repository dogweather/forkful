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

## 왜?
HTML을 파싱하는 과제는 웹 개발에서 중요한 부분입니다. HTML 문서를 이해하고 웹페이지의 내용을 추출하여 웹 애플리케이션에 동적으로 표시하는 것은 필수적입니다. Gleam을 사용하면 HTML 파싱을 쉽고 효율적으로 수행할 수 있습니다.

## 어떻게?
Gleam은 HTML 파싱에 유용한 라이브러리를 제공합니다. 다양한 함수를 사용하여 HTML 태그를 추출하고 내용을 가져오는 것이 가능합니다. 아래는 예시 코드와 그 결과입니다.

```Gleam
import gleam/html

let html = "<div>Hello Gleam!</div>"

// <div> 태그를 가져오기
let div = html |> html.Elements.get_first_by_tag("div")
// div 태그의 내용 가져오기
let content = div |> html.Elements.get_text

// 결과 출력
gleam/core/format!("{content}") // "Hello Gleam!"
```
아래는 좀 더 복잡한 예제 코드와 그 결과입니다.

```Gleam
import gleam/http
import gleam/html

let url = "https://gleam.run"
// HTTP 요청 보내기
let response = url
|> http.get
// HTTP 응답 바디 가져오기
let body = response.body
// Gleam Document로 변환하기
let document = body |> html.parse
// Document에서 <a> 태그 추출하기
let anchors = document |> html.find_all_by_tag("a")
// 모든 <a> 태그의 속성 가져오기
let attrs = anchors
|> List.map(\link -> link |> html.Element.get_attributes)
// 결과 출력
attrs // 예: [{| href => "/docs/why-gleam", |}, {| href => "/downloads", |}, ...]
```

## 깊게 들어가기
Gleam의 `html` 라이브러리는 HTML 태그를 레코드로 바로 가져올 수 있어서 매우 편리합니다. 또한 Gleam의 스트림을 사용하여 HTML 문서를 파싱할 수도 있습니다. 더 자세한 내용은 공식 문서를 참조해주세요.

## 더 읽어보기
- [Gleam 공식 문서](https://gleam.run)
- [Gleam 코드 저장소](https://github.com/gleam-lang/gleam)