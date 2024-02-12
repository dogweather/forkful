---
title:                "HTTP 요청 보내기"
date:                  2024-01-20T18:00:36.958260-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTP 요청 보내기"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (무엇인가요? 왜 사용하나요?)
HTTP 요청 보내기는 서버에 어떤 데이터를 요청하거나 서버의 리소스를 조작할 때 사용합니다. 프로그래머들은 API 통신, 데이터 수집, 웹 서비스와 상호 작용하기 위해 이 기능을 사용합니다.

## How to: (어떻게 하나요?)
Rust에서 하는 일반적인 HTTP GET 요청 예시입니다:

```rust
use reqwest; // Reqwest 크레이트 사용
use std::error::Error;

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>>{
    let res = reqwest::get("https://httpbin.org/get").await?;
    let body = res.text().await?;
    println!("Response: {}", body);
    Ok(())
}
```

이렇게 하면 요청의 결과인 일부 텍스트가 출력됩니다.

## Deep Dive (심화 학습)
HTTP 요청을 보내는 것은 웹의 기본 기능 중 하나입니다. 1991년 HTTP 표준이 처음 소개된 이래로 프로토콜은 계속 진화해 왔죠. Rust에서는 `reqwest`와 같은 여러 크레이트가 HTTP 통신을 간단하게 만듭니다. `hyper`는 `reqwest`의 기반이 되는 낮은 수준의 HTTP 구현이며 직접 다루고 싶은 경우에 사용할 수 있습니다.

## See Also (참고 자료)
- Reqwest documentation: [https://docs.rs/reqwest/](https://docs.rs/reqwest/)
- Hyper documentation: [https://hyper.rs/](https://hyper.rs/)
- HTTP on MDN: [https://developer.mozilla.org/en-US/docs/Web/HTTP](https://developer.mozilla.org/en-US/docs/Web/HTTP)
