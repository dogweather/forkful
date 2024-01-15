---
title:                "HTML 파싱하기"
html_title:           "Rust: HTML 파싱하기"
simple_title:         "HTML 파싱하기"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/parsing-html.md"
---

{{< edit_this_page >}}

## 왜

웹 개발에서 HTML은 매우 중요한 역할을 합니다. 웹 사이트를 구축하는 데 필수적인 요소이기 때문에 HTML을 잘 다루는 것은 개발자에게 매우 유용합니다. 그래서 우리는 HTML을 파싱하는 방법을 배우고 싶습니다.

## 어떻게

HTML 파싱을 위해 우리는 Rust 언어를 사용할 것입니다. 이 언어는 메모리 안전성과 속도를 보장해주는 장점이 있습니다. 우리는 Rust 커뮤니티에서 강력한 패키지인 `html5ever`를 사용할 것입니다.

먼저, `Cargo.toml` 파일에 다음과 같이 패키지를 추가해주세요.

```Rust
[dependencies]
html5ever = "0.24.0"
```

다음으로, `main.rs` 파일에서 다음 코드를 추가해주세요.

```Rust
extern crate html5ever;
use html5ever::parse_document;
use html5ever::rcdom::{RcDom, Handle};
use html5ever::tendril::TendrilSink;

fn main() {
    // HTML 문자열을 불러옵니다.
    let html = String::from("<html><body><h1>Hello, Rust!</h1></body></html>");
    // HTML 문서를 파싱합니다.
    let document = parse_document(RcDom::default(), Default::default())
        // HTML 문자열을 읽어들입니다.
        .from_utf8()
        // 읽어들인 문자열을 Tendril로 변환합니다.
        .read_from(&mut html.as_bytes())
        .unwrap();
    
    // h1 태그를 선택합니다.
    let h1 = get_element_by_tag_name(&document, "h1");
    // 결과를 출력합니다.
    println!("{}", h1);
}

// 태그 이름을 기준으로 요소를 선택하는 함수
fn get_element_by_tag_name<'a>(document: &'a Handle, tag_name: &str) -> &'a str {
    // 요소를 담을 변수를 생성합니다.
    let mut element = "";
    // document의 모든 자식 요소를 반복문으로 검사합니다.
    for child in document.children.borrow().iter() {
        match child.data {
            // 태그 이름이 일치하는 요소를 찾으면 element 변수에 넣고 반복문을 종료합니다.
            Element(ref name, _) if name.local.as_ref() == tag_name => {
                element = name.local.as_ref();
                break;
            },
            // 일치하는 요소를 찾지 못하면 다음 자식 요소를 검사합니다.
            _ => (),
        };
    }
    // 선택된 요소를 반환합니다.
    element
}
```

위 코드를 실행하면 `h1` 태그를 선택하고 해당 태그의 내용을 출력하게 될 것입니다.

```
h1 태그의 내용인 "Hello, Rust!"가 나옵니다.
```

## 딥 다이브

우리는 `html5ever` 패키지를 사용하여 HTML 문서를 파싱하였습니다. 이 패키지는 W3C의 공식 HTML 파서를 Rust로 포팅해 놓은 것입니다. 이 기능을 사용하면, HTML 문서의 여러 태그를 선택하고 원하는 데이터를 추출하는 등 다양한 작업을 할 수 있습니다. 또한, Rust 언어의 안전성과 속도를 유지하면서 웹 개발에 필요한 기능을 수행할 수 있게 됩니다.

## 참고 자료

- [Rust 언어 공식 홈페이지](https://www.rust-lang.org/ko/)
- [html5ever 패키지 공식 문서](https://docs.rs/html5ever/0.24.0/html5ever/)
- [W3C 공식 HTML5 파서](https://www.w3.org/TR/2011/WD-html5-20110525/parsing.html)