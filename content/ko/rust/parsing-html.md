---
title:                "HTML 파싱"
html_title:           "Fish Shell: HTML 파싱"
simple_title:         "HTML 파싱"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/parsing-html.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

HTML 파싱이란 일련의 문자열을 의미 있는 요소로 해석하는 것입니다. 프로그래머들이 이를 수행하는 이유는 HTML 문서에서 데이터를 추출하거나, 페이지의 특정 섹션에 접근하기 위해서입니다.

## 어떻게:

Rust에서는 `scraper`라는 크레이트를 사용하여 HTML 파싱을 할 수 있습니다. 아래는 기본적인 사용 방법을 보여주는 코드입니다:

```Rust
use scraper::{Html, Selector};

fn main() {
    let html = r#"<p class='foo'>Hello, world!<p>"#;
    let document = Html::parse_document(html);
    let selector = Selector::parse(".foo").unwrap();

    for element in document.select(&selector) {
        println!("{:?}", element.value().attr("class").unwrap());
    }
}
```
이 코드를 실행하면, 파싱된 HTML 문서에서 선택한 클래스 요소를 찾아 출력합니다.

## Deep Dive: 

1. HTML 파싱은 웹의 초창기부터 있었습니다. 웹 개발이 발전함에 따라, HTML 파싱 기법과 도구는 점차 정교해졌습니다.
2. 단순 파싱 외에도, 웹 접근성을 높이는 데 도움이 되는 DOM 파싱, SAX 파싱 등의 เ안법도 있습니다.
3. Rust에서 HTML 파싱을 구현할 때, `Html`와 `Selector` 구조체를 이용합니다. `Html::parse_document()` 함수는 HTML 문자열을 문서로 변환하고, `Selector::parse()` 함수는 CSS 셀렉터를 파싱합니다.

## 참고 자료:

- Rust `scraper` 크레이트 문서: https://docs.rs/scraper/0.12.0/scraper/
- HTML 파싱에 대한 MDN 가이드: https://developer.mozilla.org/en-US/docs/Web/HTML/Parser 
- 파싱 알고리즘에 대한 Mozilla Hacks 블로그: https://hacks.mozilla.org/2015/04/inside-the-brackets-html-parsing/