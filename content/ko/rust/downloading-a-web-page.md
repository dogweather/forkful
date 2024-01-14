---
title:                "Rust: 웹 페이지 다운로드하기"
simple_title:         "웹 페이지 다운로드하기"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 왜

웹 페이지를 다운로드하는 과정은 우리가 인터넷을 사용하는 일상 속에서 필수적인 동작입니다. Rust 프로그래밍을 통해 이 과정을 더욱 안정적이고 효율적으로 할 수 있습니다. 이 글에서는 Rust 언어를 사용하여 웹 페이지를 다운로드하는 방법에 대해 알아보겠습니다.

## 어떻게

Rust는 안전한 메모리 관리와 빠른 속도를 지원하기 때문에 웹 페이지를 다운로드하는 과정에 매우 적합합니다. 먼저 `reqwest`라이브러리를 다운로드해야 합니다. 그런 다음 `main` 함수 내에 다음 코드를 추가합니다:

```Rust
use reqwest::Error;

#[tokio::main]
async fn main() -> Result<(), Error> {
    let res = reqwest::get("https://www.example.com").await?;
    println!("Status: {}", res.status());

    let body = res.text().await?;
    println!("Body:\n\n{}", body);

    Ok(())
}
```

이 예제 코드는 `reqwest` 라이브러리를 사용하여 `https://www.example.com`에서 웹 페이지를 다운로드하고, 다운로드 받은 페이지의 상태와 내용을 출력합니다. 출력 결과는 다음과 같습니다:

```
Status: 200 OK

Body:

<html>
<head>
  <title>Example Domain</title>
  ...
</head>
<body>
...
<h1>Example Domain</h1>
<p>This domain is for use in illustrative examples in documents. You may use this
domain in literature without prior coordination or asking for permission.</p>
...
</body>
</html>
```

## 깊게 파고들기

더 복잡한 웹 페이지를 다운로드할 때 요청에 필요한 추가적인 설정이 필요할 수도 있습니다. 예를 들어, 인증이 필요한 경우 해당 페이지에 대한 로그인 정보를 포함한 요청이 필요합니다. `reqwest` 라이브러리는 이러한 설정을 관리하는 다양한 기능을 제공합니다. 자세한 내용은 [공식 문서](https://docs.rs/reqwest/latest/reqwest/)를 참조하시기 바랍니다.

## 보기

다른 실제 예제들과 더 많은 정보를 원하신다면, [공식 Rust 웹사이트](https://www.rust-lang.org/learn/get-started)와 [Rust 프로그래밍 언어 책](https://doc.rust-lang.org/book/)을 살펴보시기 바랍니다.

## 참고

- [Rust 언어 공식 웹사이트](https://www.rust-lang.org/)
- [`reqwest` 라이브러리 공식 문서](https://docs.rs/reqwest/latest/reqwest/)