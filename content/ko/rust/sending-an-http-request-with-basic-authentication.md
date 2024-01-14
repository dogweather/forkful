---
title:                "Rust: 기본 인증으로 http 요청 보내기"
simple_title:         "기본 인증으로 http 요청 보내기"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# 왜 HTTP 요청에 기본 인증을 사용하는가?

HTTP 요청을 보낼 때 기본 인증을 사용하는 이유는 보안이 가장 큰 이유입니다. 기본 인증은 사용자 이름과 비밀번호를 인코딩하여 서버에게 전송하고, 서버는 이를 디코딩하여 사용자를 인증하는 방식입니다.

## 어떻게 사용하는가?

우리는 Rust 언어를 사용하여 기본 인증을 이용해 HTTP 요청을 보낼 수 있습니다. 다음은 다양한 방식으로 기본 인증을 구현하는 코드 예제입니다.

```
use reqwest::{Client, Error};
use reqwest::header::{HeaderValue, BASIC_AUTH};

#[tokio::main]
async fn main() -> Result<(), Error> {
    let client = Client::builder().build()?;

    // 기본 인증 헤더 생성
    let auth = BASIC_AUTH
        .header_value("username", "password")?
        .clone();

    // GET 요청에 기본 인증 추가
    let resp = client.get("https://www.example.com")
        .header(auth)
        .send()
        .await?;

    println!("{:#?}", resp.text().await?);

    Ok(())
}
```

마찬가지로 curl 명령어를 사용하여 기본 인증을 사용할 수도 있습니다.

```
curl "https://www.example.com" -H "Authorization: Basic dXNlcm5hbWU6cGFzc3dvcmQ="
```

## 깊이 들어가기

기본 인증은 매우 간단하고 기본적인 방식의 인증이므로 상대적으로 보안이 취약합니다. 따라서 HTTPS를 통해 암호화된 연결을 사용하여 기본 인증을 함께 사용하는 것을 권장합니다. 또한 사용자 이름과 비밀번호를 인코딩하여 전송하기 때문에 다른 인증 방식에 비해 보안이 취약할 수 있습니다.

# 또 다른 정보

- [Rust 공식 홈페이지](https://www.rust-lang.org/ko)
- [reqwest crate 공식 문서](https://docs.rs/reqwest/0.11.2/reqwest/)