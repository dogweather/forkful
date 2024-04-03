---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:58.910072-07:00
description: "\uBC29\uBC95: Rust\uC5D0\uC11C HTML\uC744 \uD30C\uC2F1\uD558\uAE30 \uC704\
  \uD574\uC11C \uC885\uC885 `scraper` \uD06C\uB808\uC774\uD2B8\uB97C \uC0AC\uC6A9\uD558\
  \uAC8C \uB429\uB2C8\uB2E4. \uC774\uB294 HTML \uBB38\uC11C\uB97C \uC21C\uD68C\uD558\
  \uACE0 \uC870\uC791\uD558\uB294 \uB370 \uD544\uC694\uD55C \uACE0\uC218\uC900 \uC778\
  \uD130\uD398\uC774\uC2A4\uB97C \uC81C\uACF5\uD569\uB2C8\uB2E4. \uCCAB\uC9F8, `Cargo.toml`\uC5D0\
  \ `scraper`\uB97C \uCD94\uAC00\uD558\uC138\uC694."
lastmod: '2024-03-13T22:44:54.912008-06:00'
model: gpt-4-0125-preview
summary: "Rust\uC5D0\uC11C HTML\uC744 \uD30C\uC2F1\uD558\uAE30 \uC704\uD574\uC11C\
  \ \uC885\uC885 `scraper` \uD06C\uB808\uC774\uD2B8\uB97C \uC0AC\uC6A9\uD558\uAC8C\
  \ \uB429\uB2C8\uB2E4."
title: "HTML \uD30C\uC2F1"
weight: 43
---

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
