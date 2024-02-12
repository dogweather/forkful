---
title:                "기본 인증을 사용한 HTTP 요청 보내기"
aliases:
- /ko/rust/sending-an-http-request-with-basic-authentication/
date:                  2024-01-20T18:02:47.781614-07:00
model:                 gpt-4-1106-preview
simple_title:         "기본 인증을 사용한 HTTP 요청 보내기"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why? (무엇이며, 왜?)
HTTP 기본 인증과 함께 HTTP 요청을 보내는 건 사용자 이름과 비밀번호로 보안된 리소스에 접근하기 위해서입니다. 서버는 계정 정보를 확인하고 요청된 작업을 수행하게 됩니다.

## How to: (어떻게:)
```Rust
// 외부 크레이트를 사용합니다: reqwest
// Cargo.toml에 reqwest를 추가하세요
// [dependencies]
// reqwest = { version = "0.11", features = ["blocking", "json"] }

use reqwest::header::{Authorization, Basic};
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    let client = reqwest::blocking::Client::new();
    let username = "user";
    let password = "pass";

    let response = client
        .get("http://example.com")
        .header(Authorization(Basic { username: username.into(), password: Some(password.into()) }))
        .send()?;

    println!("Status: {}", response.status());
    println!("Body:\n{}", response.text()?);
    Ok(())
}
```

예상 출력:
```
Status: 200 OK
Body:
response from server
```

## Deep Dive (심층 분석)
기본 인증(Basic Authentication)은 HTTP의 초기 인증 메커니즘 중 하나입니다. 요청 헤더에 사용자 이름과 비밀번호를 인코딩하여 포함시킵니다. 취약점이 있으므로 HTTPS를 통해 보안을 강화하는 것이 중요합니다. 대안으로 OAuth, API 키 등이 사용될 수 있습니다. Rust에서는 `reqwest`와 같은 크레이트를 사용하여 인증 프로세스를 손쉽게 처리할 수 있는데, 이는 내부적으로 `Authorization` 헤더를 구성하여 서버로 보냅니다.

## See Also (추가 정보)
- [Reqwest Documentation](https://docs.rs/reqwest/) - 요청을 보내는 데 도움이 되는 Reqwest 크레이트 문서
- [HTTP Basic Auth RFC](https://tools.ietf.org/html/rfc7617) - 기본 인증에 대한 공식 문서
- [Mozilla Developer Network - HTTP authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication) - HTTP 인증에 대한 개념적 이해를 돕는 자료
