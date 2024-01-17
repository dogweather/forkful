---
title:                "경석HTML 작업"
html_title:           "Rust: 경석HTML 작업"
simple_title:         "경석HTML 작업"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/parsing-html.md"
---

{{< edit_this_page >}}

오늘 우리는 HTML 파싱에 대해 이야기할 것입니다. HTML 파싱이란 HTML 문서에서 데이터를 추출하는 과정입니다. 프로그래머들은 주로 웹 스크래핑, 웹 크롤링 및 웹 데이터 마이닝과 같은 작업을 위해 HTML 파싱을 합니다.

## What & Why?
HTML 파싱은 HTML 문서에서 데이터를 추출하는 과정입니다. 웹 페이지의 개별 요소를 식별하고 원하는 데이터를 추출하기 위해 필요합니다. 이를 통해 프로그래머들은 웹 사이트에서 원하는 정보를 손쉽게 가져올 수 있습니다.

## How to:
Rust는 HTML 파싱을 수행하기 위해 여러 라이브러리를 제공합니다. 가장 인기있는 라이브러리는 `html5ever`입니다. 이 라이브러리는 HTML5 판별자와 연결된 파서를 제공합니다. 아래는 HTML 문서에서 `<title>` 태그 안의 텍스트를 추출하는 예제입니다.

```Rust
use html5ever::parse_document;
use html5ever::rcdom::{Handle, RcDom};
use html5ever::tendril::TendrilSink;
use tendril::stream::TendrilSink;

let input = r#"
  <html>
    <head>
      <title>Hello, world!</title>
    </head>
    <body>
      <h1>Hello, Rust!</h1>
    </body>
  </html>
"#;

let dom = parse_document(RcDom::default(), Default::default())
  .from_utf8()
  .read_from(&mut input.as_bytes());
let window = dom.document.children.borrow()[0].clone();

for child in window.children.borrow().iter() {
  if let Handle::Text(text) = child.data {
    println!("{}", text);
  }
}

// Output:
// Hello, world!
```

## Deep Dive:
HTML 파싱에는 여러 가지 라이브러리와 방법이 있지만, Rust에서는 `html5ever`와 `kuchiki`가 가장 인기가 있습니다. `html5ever`는 W3C HTML5 파서 스펙을 따르는 라이브러리이며, `kuchiki`는 jQuery와 유사한 CSS 선택기 기능을 제공합니다.

`html5ever`는 기본적으로 HTML5 문서를 처리하는 파서를 제공하지만, 다른 문서 유형의 처리를 위한 설정도 제공할 수 있습니다. `kuchiki`는 HTML 파서를 사용하여 일반 DOM 트리를 제공합니다. 이 두 라이브러리는 각각의 특징을 가지고 있으므로 프로젝트 요구 사항에 따라 선택해야 합니다.

## See Also:
- [html5ever 문서](https://docs.rs/html5ever/)
- [kuchiki 레포지토리](https://github.com/kuchiki-rs/kuchiki)