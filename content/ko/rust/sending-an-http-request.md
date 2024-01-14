---
title:                "Rust: http 요청 보내기"
simple_title:         "http 요청 보내기"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 왜

HTTP 요청을 보내는 것에 대해 관심을 가질 이유는 무엇일까요? 먼저, HTTP 요청은 웹에서 가장 많이 사용되는 프로토콜 중 하나입니다. 따라서 웹 어플리케이션을 개발하거나 데이터를 받아오는 등 웹 개발을 할 때 빈번하게 사용되게 됩니다. 또한, Rust 언어는 안정성과 높은 성능을 자랑하는 언어로, HTTP 요청을 보내는 작업에도 적합합니다.

## 방법

아래 코드 블록에서는 Rust 언어를 이용하여 간단한 HTTP 요청을 보내는 방법을 살펴보겠습니다. 먼저, `reqwest` 라이브러리를 사용하여 HTTP 클라이언트를 생성합니다. 그리고 생성한 클라이언트를 이용하여 원하는 URL로 GET 요청을 보내고, 해당 요청의 응답을 받아 옵니다. 이렇게 받아온 응답은 `Response` 타입으로, 여러가지 정보를 포함하고 있습니다. 이를 이용하여 응답의 상태 코드를 확인하거나, 응답 바디의 데이터를 추출할 수 있습니다.

```Rust
use reqwest::Error;

async fn send_request() -> Result<(), Error> {
    // HTTP 클라이언트 생성
    let client = reqwest::Client::new();

    // GET 요청 보내기
    let response = client.get("https://www.example.com").send().await?;

    // 응답 상태 코드 확인
    if response.status().is_success() {
        println!("Request was successful!");
    }

    // 응답 바디를 String으로 변환하여 출력
    let body = response.text().await?;
    println!("Body: {}", body);

    Ok(())
}
```

위의 코드를 실행하면, `Request was successful!` 문구와 함께 해당 URL의 HTML 코드가 출력될 것입니다.

## 깊이 파고들기

`reqwest` 라이브러리는 다양한 기능을 제공하며, 좀 더 복잡한 HTTP 요청을 보내는 것도 가능합니다. 예를 들어, POST 요청을 보내거나, 요청의 파라미터를 지정하는 등의 작업을 할 수 있습니다. 또한 `async/await` 문법을 이용하여 비동기적으로 요청을 보낼 수도 있습니다.

## 참고 자료

- Rust 공식 홈페이지: https://www.rust-lang.org/ko
- reqwest 라이브러리 문서: https://docs.rs/reqwest/0.11.2/reqwest/
- HTTP 클라이언트를 이용한 요청 보내기 예제: https://mattgathu.github.io/rust/programming/2018/04/28/rust-http-request-example.html

## 참고

- [Rust 공식 홈페이지](https://www.rust-lang.org/ko)에서 다양한 Rust 관련 자료를 찾아볼 수 있습니다.
- `reqwest` 라이브러리의 문서에는 더 많은 기능과 예제들이 제공되어 있으니 참고하시면 도움이 될 것입니다.
- 위의 예제 링크에서는 더 많은 HTTP 요청 예제들을 확인할 수 있습니다.