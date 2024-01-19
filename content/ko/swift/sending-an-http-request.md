---
title:                "HTTP 요청 보내기"
html_title:           "Clojure: HTTP 요청 보내기"
simple_title:         "HTTP 요청 보내기"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 무엇과 왜?

HTTP 요청이란 웹 서버에 정보를 요청하는 방법입니다. 프로그래머는 API에서 데이터를 가져오거나 웹 서버에 데이터를 전송하는 등 다양한 목적으로 HTTP 요청을 사용합니다.

## 작성방법:

아래는 Swift에서 HTTP 요청을 보내는 예제 코드입니다.

```Swift
import Foundation

let url = URL(string: "https://example.com")!
let task = URLSession.shared.dataTask(with: url) { data, response, error in
    if let error = error {
        print("Error: \(error)")
    } else if let data = data {
        print(String(data: data, encoding: .utf8) ?? "")
    }
}
task.resume()
```

이 코드를 실행하면 "https://example.com" 주소의 웹 서버에 GET 요청을 보내게되고, 결과를 출력합니다.

## 깊이있게 알아보기

HTTP 요청은 1991년에 처음 생성된 HTTP(HTTP/1.1의 기본이되는) 에서부터 존재합니다. Swift에서는 URLSession, URLRequest 등의 클래스를 통해 HTTP 요청을 처리할 수 있습니다.

수많은 대안들이 있지만 URLSession은 쉽고 안전하게 사용할 수 있으며, 비동기 작업에 강력한 기능을 제공하기 때문에 Swift 개발자들이 주로 사용합니다.

이 코드의 구현 세부사항으로는 URLSession.shared를 이용하여 shared singleton session을 만들고, dataTask(with: completionHandler:) 메서드를 사용하여 데이터 작업을 만들고 시작하는 것입니다.

## 참고 자료

- Swift 공식 문서 URLSession: [https://developer.apple.com/documentation/foundation/urlsession](https://developer.apple.com/documentation/foundation/urlsession)
- HTTP 소개 (MDN): [https://developer.mozilla.org/ko/docs/Web/HTTP/Overview](https://developer.mozilla.org/ko/docs/Web/HTTP/Overview)