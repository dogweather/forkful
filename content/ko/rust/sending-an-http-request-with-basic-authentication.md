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

## 뭐고 왜하는거지? 
HTTP 요청에 기본 인증을 사용하여 요청을 보내는 것은 인터넷에서 데이터를 안전하고 신뢰할 수 있게 만드는 기술입니다. 이 기술을 사용하는 이유는 우리가 자주 사용하는 웹 응용 프로그램에서 보안과 관련된 문제를 해결하기 위해서입니다.

## 하는 법: 
```Rust
use reqwest;
use std::env;
use reqwest::header::{Authorization, BASIC};

let client = reqwest::Client::new();
let username = "example";
let password = "password";
let url = "https://www.example.com/api";

// 기본 인증 해더를 만들고 HTTP 클라이언트를 설정합니다.
let mut req = reqwest::Request::new(reqwest::Method::GET, reqwest::Url::parse(url).unwrap());
req.headers_mut().set(Authorization(BASIC.auth(&username, &Some(password)).unwrap()));

// 오류가 없고 전달 된 자격 증명이 올바른지 확인합니다.
let resp = match client.execute(req) {
    Ok(resp) => resp,
    Err(e) => panic!("failed to execute request: {}", e),
}

// API 서버에서 반환 된 응답의 상태 코드를 확인합니다.
// 상태 코드 200은 성공적인 요청을 나타내며, 다른 상태 코드는 오류를 의미합니다.
if resp.status().is_success() {
    println!("{:?}", resp.text().unwrap());
} else {
    println!("Error: {}", resp.status());
}
```

## 더 깊게 생각해보기: 
(1) 인터넷의 초기에는 인증 방식이 없었기 때문에, 개인 정보의 안전성이 보장되지 않았습니다. (2) 기본 인증 외에 인증 방법으로는 OAuth, JWT 등이 있으며, 각각의 장단점이 있습니다. (3) HTTP 요청의 일부로 기본 인증을 포함하는 방법은 간단하고 직관적입니다. 기본 인증은 사용자 이름과 비밀번호를 Base64 형식으로 인코딩하여 전달합니다.

## 더 알아보기: 
- Rust에서 HTTP 요청을 보내는 방법: https://crates.io/crates/reqwest
- HTTP 인증의 다른 방식: https://blog.restcase.com/4-most-used-rest-api-authentication-methods/
- HTTP 기본 인증의 구현 및 보안 취약점: https://help.accuwebhosting.com/article/how-to-enable-http-basic-authentication-in-iis-windows-server-2012