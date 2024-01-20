---
title:                "HTTP 요청 보내기"
html_title:           "Clojure: HTTP 요청 보내기"
simple_title:         "HTTP 요청 보내기"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

HTTP 요청을 보내는 것은 클라이언트가 서버에게 데이터를 요청하는 방법입니다. 프로그래머들은 데이터를 교환하거나 웹 서비스와 상호작용하기 위해 이것을 사용합니다.

## 어떻게: 

```Rust 
// reqwest 라이브러리를 사용
extern crate reqwest;

use reqwest::Error;

async fn get_data() -> Result<(), Error> {

    // 서버에 요청 보내기
    let response = reqwest::get("https://httpbin.org/ip").await?;

    // 응답을 문자열로 전환
    let body = response.text().await?;

    // 응답 출력하기
    println!("body = {:?}", body);
    Ok(())
}
```

위의 코드는 "https://httpbin.org/ip" URL에 HTTP GET 요청을 보내서 응답을 문자열로 변환하고 이를 출력하는 간단한 예제입니다.

## 딥 다이브 :

HTTP 요청은 웹이 상호작용하는 기본적인 방법이며 HTTP라는 프로토콜에서 시작되었습니다. 현재 Rust에서는 reqwest 외에도 hyper, surf 등 다양한 라이브러리를 이용하여 HTTP 요청을 보낼 수 있습니다. 또한, 이러한 라이브러리에서 제공하는 메서드를 사용하여 요청 유형(GET, POST 등), 헤더 정보 등을 상세하게 지정할 수 있습니다.

## 참고 자료 : 

1. Rust의 reqwest 라이브러리 : https://docs.rs/reqwest/0.11.4/reqwest/
2. How To Send HTTP Requests in Rust : https://www.section.io/engineering-education/http-requests-in-rust/
3. Making HTTP Requests in Rust : https://blog.logrocket.com/making-http-requests-in-rust/

이 정보를 활용하여 Rust에서 더 효과적인 HTTP 요청을 할 수 있게 되어 기쁩니다!