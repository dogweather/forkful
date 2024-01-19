---
title:                "기본 인증을 사용하여 http 요청 보내기"
html_title:           "Bash: 기본 인증을 사용하여 http 요청 보내기"
simple_title:         "기본 인증을 사용하여 http 요청 보내기"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

HTTP 요청에 기본 인증을 함께 보내는 것은 사용자 이름과 비밀번호를 사용해 서버와 안전하게 통신하는 방법입니다. 프로그래머들이 이를 실행하는 이유는 서버에서 보내는 데이터를 안전하게 유지하여 민감한 정보를 보호하기 위해서입니다.

## 어떻게 작동하는가:

다음은 Rust에서 기본 인증을 사용하여 HTTP 요청을 보내는 표본 코드입니다:

```Rust
use reqwest::header::{HeaderMap, HeaderValue, AUTHORIZATION, USER_AGENT};
use base64::encode;

let url = "http://httpbin.org/basic-auth/user/passwd";
let user = "user";
let passwd = "passwd";

let auth = format!("{}:{}", user, passwd);
let auth = format!("Basic {}", encode(auth));

let mut headers = HeaderMap::new();
headers.insert(USER_AGENT, HeaderValue::from_static("Rust"));
headers.insert(AUTHORIZATION, HeaderValue::from_str(&auth).unwrap());

let resp = reqwest::Client::new()
    .get(url)
    .headers(headers)
    .send()
    .await?;

assert!(resp.status().is_success());
```

## 깊이 들어가보기:

Rust에서 HTTP 요청에 기본 인증을 보내기 위해 예전에는 hyper나 curl과 같은 라이브러리를 사용하곤 했습니다. 이러한 라이브러리들도 여전히 유효하지만, 최근에는 reqwest 라이브러리가 더욱 인기를 얻고 있습니다. 이는 비동기 요청에 대한 지원과 함께 다양한 편의 기능을 제공하기 때문입니다. 

대안적으로, 대신 직접 헤더를 설정하여 인증을 위한 구현을 할 수 있습니다. 그러나 이는 보안사항에서 신중히 다뤄야 하며, 라이브러리를 사용함으로써 완화할 수 있는 실수를 범할 가능성이 더 높습니다.

## 참고 자료:

- [reqwest 라이브러리](https://docs.rs/reqwest)
- [Rust로 HTTP 요청 보내기](https://dev.to/sean_lawless/build-a-concurrent-http-server-in-rust-2o4k)
- [Rust Basic Authentication](https://stackoverflow.com/questions/43023205/how-do-you-set-the-authorization-header-in-rust-reqwest)