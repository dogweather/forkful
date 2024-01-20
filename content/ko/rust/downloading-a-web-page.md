---
title:                "웹 페이지 다운로드하기"
html_title:           "Bash: 웹 페이지 다운로드하기"
simple_title:         "웹 페이지 다운로드하기"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

웹페이지 다운로드란 서버로부터 HTML, CSS, 이미지 등 웹 페이지 구성 요소를 로컬에 받아오는 것입니다. 이는 웹 데이터 크롤링, 테스트 데이터 수집 등 프로그래밍 작업에 익살스럽게 활용됩니다.

## 어떻게 하는가:

Rust를 이용해 웹페이지를 다운로드하는 가장 간단한 예시는 `reqwest` 패키지를 이용하는 것입니다.

```rust
use reqwest;

#[tokio::main]
async fn main() -> Result<(), reqwest::Error> {
    let content = reqwest::get("https://www.google.com")
        .await?
        .text()
        .await?;
    println!("{}", content);

    Ok(())
}
```

이 코드를 실행하면 google.com의 HTML 페이지를 다운로드 받아 콘솔에 출력할 수 있습니다.

## 심층 탐구:

웹페이지 다운로드는 월드 와이드 웹이 폭발적으로 성장하면서 중요한 개념으로 자리잡았습니다. 다양한 언어와 라이브러리가 이를 쉽게 구현할 수 있도록 도와줍니다. Rust에서는 위에서 살펴본 `reqwest` 외에도 `hyper`, `isahc`와 같은 패키지들이 있습니다. 

이런 라이브러리들은 대부분 내부적으로 HTTP/HTTPS 프로토콜을 통한 요청-응답 모델을 구현하고 있습니다. 자세한 분석과 개인화를 위해 이런 라이브러리들의 소스 코드를 확인하는 것도 좋은 학습 방법입니다.

## 참고 문헌:

- Rust `reqwest`: https://docs.rs/reqwest/
- Rust `hyper`: https://docs.rs/hyper/
- Rust `isahc`: https://docs.rs/isahc/
- HTTP/HTTPS 프로토콜: https://developer.mozilla.org/ko/docs/Web/HTTP/Overview