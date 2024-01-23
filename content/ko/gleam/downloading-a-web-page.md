---
title:                "웹 페이지 다운로드하기"
date:                  2024-01-20T17:43:53.602522-07:00
model:                 gpt-4-1106-preview
simple_title:         "웹 페이지 다운로드하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
웹 페이지 다운로드는 인터넷에서 HTML 문서를 가져오는 과정입니다. 프로그래머들은 데이터 분석, 웹 스크래핑, 자동화 도구 개발 등을 위해 이 작업을 수행합니다.

## How to: (방법)
Gleam에서 웹 페이지를 다운로드하기 위해 `httpc` 모듈을 사용하겠습니다. 간단한 예제 코드를 보여드리죠.

```gleam
import gleam/io
import gleam/http/httpc
import gleam/http

pub fn main() {
  case httpc.get("https://example.com") {
    Ok(response) -> io.println(response.body)
    Error(error) -> io.println("Failed to download the page: ", error)
  }
}
```

실행 결과는 다음과 같습니다:

```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
```

## Deep Dive (심화학습)
과거에는 라이브러리 없이 웹 페이지를 다운로드하려면 직접 HTTP 프로토콜을 구현해야 했습니다. 이제는 `httpc`와 같은 모듈이 작업을 쉽게 해줍니다. Gleam의 `httpc`는 내부적으로 Erlang의 HTTP 클라이언트를 사용합니다. 웹 페이지를 다운로드하는 다른 방법으로는 체인을 구성하는 `gleam/http` 모듈의 고급 기능을 활용할 수도 있습니다.

## See Also (참고자료)
- Gleam 공식 문서: https://gleam.run/book/
- `httpc` 모듈 문서: https://hexdocs.pm/gleam_httpc/
- `http` 모듈 문서: https://hexdocs.pm/gleam_http/
