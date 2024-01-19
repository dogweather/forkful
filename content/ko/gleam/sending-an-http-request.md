---
title:                "HTTP 요청 보내기"
html_title:           "Clojure: HTTP 요청 보내기"
simple_title:         "HTTP 요청 보내기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

HTTP 요청을 보내는 것은, 클라이언트가 서버에 정보를 요청하거나 송신하는 방법입니다. 프로그래머는 이를 통해 웹 기반 리소스를 추출하고 사용자와 상호 작용합니다.

## 사용 방법:

Gleam에서는 `gleam/httpc` 라이브러리를 통해 HTTP 요청을 보낼 수 있습니다. 아래 코드는 GET 요청의 예시입니다:

```Gleam
import gleam/httpc
import gleam/http.{Uri}

fn request_example() {
  let uri = Uri.parse("http://example.com/")
  case httpc.get(uri) {
    Ok(response) -> io.println(response.status)
    Error(e) -> io.println(e)
  }
}
```
위 코드 실행 시, '200' (성공 응답) 또는 에러 메시지가 출력됩니다.

## 더 깊게 알아보기

HTTP 요청에는 과거와 현재, 여러 가지 방법이 있습니다. 과거에는 주로 `XMLHttpRequest`를 사용했지만, 현재는 더 강력한 `Fetch API`가 일반적입니다. 

HTTP 요청의 중요한 기능 중 하나는 서버에서 동적 콘텐츠를 가져오는 것입니다. 이를 통해 사용자가 요청한 맞춤 정보가 실시간으로 표시됩니다.

다만, Gleam에서는 타입 안전성을 제공하므로, 이는 다른 언어에 비해 사용자에게 더 큰 이점을 제공합니다.

## 참고자료

1. Gleam 공식 문서: https://gleam.run/docs/
2. HTTP 클라이언트 라이브러리 문서: https://github.com/gleam-lang/http
3. Fetch API 관련 MDN 문서: https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API