---
title:                "HTML 파싱"
date:                  2024-01-20T15:33:50.704722-07:00
simple_title:         "HTML 파싱"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (무엇이며, 왜?)
HTML 파싱은 웹 페이지의 구조를 분석하고 정보를 추출하는 과정입니다. 프로그래머들은 데이터를 자동으로 수집하거나 웹 컨텐츠를 조작하기 위해 이를 수행합니다.

## How to: (방법)
Rust는 강력한 크레이트(ecosystem)를 통해 쉽게 HTML을 파싱할 수 있도록 해줍니다. `scraper` 크레이트는 이러한 작업을 위한 좋은 시작점입니다.

```Rust
use scraper::{Html, Selector};

fn main() {
    // HTML 샘플
    let html = r#"
        <ul>
            <li>아이템 1</li>
            <li>아이템 2</li>
            <li>아이템 3</li>
        </ul>
    "#;

    // 파싱할 HTML 문서
    let document = Html::parse_document(html);

    // li 태그 선택
    let selector = Selector::parse("li").unwrap();

    // 선택한 태그의 텍스트 추출
    for element in document.select(&selector) {
        let text = element.text().collect::<Vec<_>>().join("");
        println!("항목: {}", text);
    }
}
```

출력 결과:

```
항목: 아이템 1
항목: 아이템 2
항목: 아이템 3
```

## Deep Dive (심화 학습)
HTML 파싱은 웹의 초창기부터 필요한 기능이었습니다. 초기에는 정규 표현식을 사용하여 파싱하곤 했으나, 불완전하고 복잡한 HTML에는 적합하지 않았죠. `BeautifulSoup`, `Nokogiri` 같은 라이브러리는 파이썬과 루비에서 파싱을 용이하게 했습니다. Rust에서는 `scraper`와 `html5ever`와 같은 크레이트가 있어, 효율적이고 타입 안정성을 제공합니다.

Rust의 크레이트들은 HTML의 파싱 속도와 안정성에 중점을 두었습니다. `html5ever`, 예를 들면, HTML Living Standard를 준수하는 파서 파이프라인을 구축함으로써, 웹 브라우저에서 사용하는 것과 유사한 정확도를 제공합니다.

대안으로는 `regex` 크레이트를 사용한 간단한 정규 표현식 접근 방식이 있지만, 복잡한 HTML 문서에는 적합하지 않을 수 있습니다. `scraper`가 제공하는 CSS 선택자 기반 찾기 기능은 웹 개발자에게 익숙한 방법론을 제공하며, 사용하기에 더 쉽습니다.

## See Also (참고 자료)
- CSS 선택자에 대한 더 깊은 이해: [https://developer.mozilla.org/ko/docs/Learn/CSS/Building_blocks/Selectors](https://developer.mozilla.org/ko/docs/Learn/CSS/Building_blocks/Selectors)
