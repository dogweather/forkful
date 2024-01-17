---
title:                "Http 요청 보내기"
html_title:           "Rust: Http 요청 보내기"
simple_title:         "Http 요청 보내기"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/sending-an-http-request.md"
---

{{< edit_this_page >}}

# What & Why?
HTTP 요청을 보내는 것은 웹 개발에서 매우 중요한 부분입니다. 여러분이 웹 브라우저에 URL을 입력하고 엔터를 눌러 웹 페이지를 로드 할 때, 실제로는 HTTP 프로토콜을 사용하여 웹 서버로부터 해당 페이지를 요청하고 응답을 받는 과정이 일어납니다. 프로그래머들은 이러한 HTTP 요청을 통해 애플리케이션과 브라우저 간에 데이터를 교환하고 서비스를 제공하는 데 사용합니다.

# How to:
Rust 언어는 웹 개발을 위한 많은 도구들을 제공하고 있습니다. 이 중에서는 Hyper라는 라이브러리를 사용하여 HTTP 요청을 보내는 방법을 알아보겠습니다. 우선, Hyper를 프로젝트에 추가해야 합니다. 다음 코드를 `Cargo.toml` 파일에 추가합니다:
```
[dependencies]
hyper = "0.14.12"
```
그런 다음 다음과 같이 코드를 작성하고 실행하면, Google의 홈페이지에 HTTP GET 요청을 보내고, 응답을 출력하게 됩니다:
```
use hyper::Client;

fn main() {
    let client = Client::new();
    let res = client.get("https://www.google.com").send().unwrap();
    let body = res.text().unwrap();
    println!("{}", body);
}
```
이제 `cargo run` 명령을 실행하여 코드를 실행하면, Google의 홈페이지의 HTML 코드가 출력될 것입니다.

# Deep Dive:
HTTP 요청은 World Wide Web의 핵심 개념 중 하나입니다. 웹의 초기에는 웹 페이지가 정적이었기 때문에, 웹 서버에서 모든 내용을 전송하는 것이 가능했지만, 현재는 동적으로 생성되는 페이지가 많아졌기 때문에 HTTP 요청이 매우 중요해졌습니다.

Rust 언어에서도 Hyper 외에도 Reqwest나 Surf 같은 HTTP 클라이언트 라이브러리가 있으며, 다른 언어에서도 많은 HTTP 클라이언트 라이브러리들이 존재합니다.

HTTP 요청에는 여러 개의 메소드 (GET, POST, PUT 등)가 있으며, 요청 본문이나 헤더를 추가하여 요청을 조작할 수도 있습니다. 이러한 내용은 각 라이브러리의 문서를 참조하시면 됩니다.

# See Also:
- [Hyper Documentation](https://docs.rs/hyper/0.14.12/hyper/index.html)
- [Reqwest](https://docs.rs/reqwest/0.11.3/reqwest/)
- [Surf](https://docs.rs/surf/2.0.1/surf/)
- [HTTP Methods](https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods)