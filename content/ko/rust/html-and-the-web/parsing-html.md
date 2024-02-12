---
title:                "HTML 파싱"
aliases:
- /ko/rust/parsing-html/
date:                  2024-02-03T19:12:58.910072-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTML 파싱"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?

Rust에서 HTML 파싱은 HTML 문서로부터 데이터를 추출하는 작업으로, 웹 스크래핑, 데이터 추출 및 웹 크롤러 구축에 필수적입니다. 프로그래머들은 웹으로부터 정보를 자동으로 수집하거나, 웹 콘텐츠를 분석하거나, 한 플랫폼에서 다른 플랫폼으로 콘텐츠를 이전하기 위해 이 작업을 합니다.

## 방법:

Rust에서 HTML을 파싱하기 위해서 종종 `scraper` 크레이트를 사용하게 됩니다. 이는 HTML 문서를 순회하고 조작하는 데 필요한 고수준 인터페이스를 제공합니다.

첫째, `Cargo.toml`에 `scraper`를 추가하세요:

```toml
[dependencies]
scraper = "0.12.0"
```

다음은 주어진 HTML 문자열로부터 모든 링크 URL을 추출하는 간단한 예제입니다:

```rust
extern crate scraper;

use scraper::{Html, Selector};

fn main() {
    let html = r#"
    <html>
    <body>
        <a href="http://example.com/1">링크 1</a>
        <a href="http://example.com/2">링크 2</a>
    </body>
    </html>
    "#;

    let document = Html::parse_document(html);
    let selector = Selector::parse("a").unwrap();

    for element in document.select(&selector) {
        let link = element.value().attr("href").unwrap();
        println!("링크 발견: {}", link);
    }
}
```

출력:

```
링크 발견: http://example.com/1
링크 발견: http://example.com/2
```

이 예제에서, 우리는 간단한 HTML 문서를 파싱하여 모든 `<a>` 요소를 찾고 그들의 `href` 속성을 추출하여 문서의 모든 링크 URL을 출력합니다. `scraper` 라이브러리는 CSS 선택자를 사용하여 특정 요소를 선택하고 HTML 파싱을 간소화하여, Rust에서 웹 스크래핑 작업을 위한 선택지가 됩니다.
