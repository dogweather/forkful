---
title:                "http 요청 보내기"
html_title:           "Rust: http 요청 보내기"
simple_title:         "http 요청 보내기"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 왜

HTTP 요청을 보내는 것에 대해 알아보는 이유는 다양합니다. 가장 일반적인 이유는 웹 어플리케이션에서 서버와 통신을 하거나 API를 호출하기 위해서입니다. 또한, 크롤링이나 웹 스크래핑과 같은 웹 데이터를 수집하는 작업에서도 HTTP 요청을 사용합니다.

## 방법

```rust
// 기본적인 HTTP 요청을 보내는 코드
fn main() {
    let client = reqwest::blocking::Client::new();
    let response = client.get("https://www.example.com").send().unwrap();
    println!("Status Code: {}", response.status());
    println!("Headers:\n{:?}", response.headers());
    println!("Body:\n{}", response.text().unwrap());
}
```

```rust
// POST 요청을 보내고 응답을 분석하는 코드
fn main() {
    let mut params = HashMap::new();
    params.insert("username", "rust_lover");
    params.insert("password", "super_secret");
    
    let client = reqwest::blocking::Client::new();
    let response = client.post("https://www.example.com/login")
        .form(&params)
        .send()
        .unwrap();
    
    // 응답에서 JSON 데이터를 추출하는 예시
    let json: serde_json::Value = response.json().unwrap();
    println!("Login Status: {}", json["status"]);
    println!("Message: {}", json["message"].as_str().unwrap());
}
```

## 딥 다이브

HTTP 요청은 클라이언트가 서버에게 데이터를 전송하거나 서버가 클라이언트에게 데이터를 제공하는 표준화된 방법입니다. 이를 통해 웹 어플리케이션의 동적인 기능과 앱 개발에 필수적인 API 호출을 가능하게 합니다. Rust의 내장 모듈인 `std::net`을 사용하여도 HTTP 요청을 보낼 수 있지만, `reqwest` 패키지를 사용하면 더 쉽고 안전하게 관리할 수 있습니다.

## 참고 자료

- [Rust 공식 홈페이지](https://www.rust-lang.org/)
- [reqwest 패키지 문서](https://docs.rs/reqwest)
- [Rust로 웹 스크래퍼 만들기 튜토리얼](https://www.ssfcstudy.com/2021/01/09/rust-web-scraper/)