---
title:                "기본 인증을 사용하여 http 요청 보내기"
html_title:           "Rust: 기본 인증을 사용하여 http 요청 보내기"
simple_title:         "기본 인증을 사용하여 http 요청 보내기"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 왜

HTTP 요청에 기본 인증을 사용해서 보내는 것은 애플리케이션에서 API를 사용하거나 웹 서비스에 접속하기 위해 필요합니다.

## 하는 법

우선, [Reqwest](https://docs.rs/reqwest/0.10.4/reqwest/) 라이브러리를 사용하기위해 `Cargo.toml` 파일에 다음과 같이 추가해주세요:

```
[dependencies]
reqwest = "0.10.4"
```

그리고 다음과 같은 코드를 사용해서 HTTP 요청에 기본 인증을 넣을 수 있습니다:

```Rust
use reqwest;

let mut headers = reqwest::header::HeaderMap::new();

// "username:password" 형식을 Base64로 인코딩합니다
let auth = base64::encode("username:password");

// Authorization 헤더를 만들어서 추가합니다
headers.insert(
    reqwest::header::AUTHORIZATION,
    format!("Basic {}", auth).parse().unwrap(),
);

// HTTP 클라이언트를 초기화합니다
let client = reqwest::Client::new();

// 기본 인증이 추가된 요청을 보냅니다
let res = client.get("https://example.com")
    .headers(headers)
    .send()
    .await?;
```

위 코드는 [Reqwest의 기본 인증 예제](https://docs.rs/reqwest/0.10.4/reqwest/struct.RequestBuilder.html#example)를 참고해서 작성하였습니다.

출력은 다음과 같이 나타날 것입니다:

```
GET https://example.com
Accept: */*
Authorization: Basic dXNlcm5hbWU6cGFzc3dvcmQ=

...response...
```

## 깊게 파보기

기본 인증이란, HTTP 요청에서 사용자 이름과 비밀번호를 Base64로 인코딩해서 보내는 것입니다. 이는 보안 수준이 낮아서 현재 사용자 이름과 비밀번호가 요청에서 노출될 수 있다는 단점이 있습니다. 따라서 보안이 중요한 애플리케이션의 경우, 보다 안전한 인증 방식을 사용하는 것을 권장합니다.

## 참고 자료

- [Reqwest 라이브러리 문서](https://docs.rs/reqwest/0.10.4/reqwest/)
- [Base64 라이브러리 문서](https://docs.rs/base64/0.13.0/base64/)