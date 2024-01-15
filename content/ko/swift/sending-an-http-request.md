---
title:                "HTTP 요청 보내기"
html_title:           "Swift: HTTP 요청 보내기"
simple_title:         "HTTP 요청 보내기"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 왜

HTTP 요청을 보내는 것은 iOS 앱 개발에서 중요한 부분입니다. 앱은 웹 서버와 통신하여 데이터를 주고받고, 다양한 웹 서비스와 연동되어 사용자에게 더 많은 기능을 제공하기 때문입니다. 따라서 Swift 개발자는 HTTP 요청을 보내는 것을 잘 알아야 합니다.

## 하는 방법

먼저, Foundation에서 제공하는 `URLRequest` 클래스를 사용하여 HTTP 요청 객체를 만듭니다. 이 객체에는 요청을 위한 URL과 HTTP 메소드 등을 설정할 수 있습니다.

```Swift
let urlString = "https://example.com/api/users"
guard let url = URL(string: urlString) else { return }

var request = URLRequest(url: url)
request.httpMethod = "GET"
```

다음으로, URLSession을 사용하여 요청을 보내고 응답을 받아옵니다. `dataTask(with: completionHandler:)` 메소드를 호출하여 비동기적으로 요청을 처리하며, 응답의 상태 코드와 받아온 데이터를 가지고 클로저가 실행됩니다.

```Swift
let task = URLSession.shared.dataTask(with: request) { data, response, error in
    guard let data = data, let response = response as? HTTPURLResponse,
          response.statusCode == 200 else { return }
    let result = String(data: data, encoding: .utf8)
    print(result) // "이 서버에서 받아온 데이터를 처리합니다."
}
task.resume()
```

## 깊이 파고들기

HTTP 요청에는 GET, POST, PUT, DELETE 등 다양한 메소드가 있으며, 다른 웹 서비스에서는 더 많은 커스텀 메소드를 사용하기도 합니다. 또한, 요청과 응답에는 헤더와 바디의 데이터 형식을 지정할 수 있으며, 인증 등의 추가적인 설정도 가능합니다.

따라서 HTTP 요청을 다루기 전에는 사용할 웹 서비스의 API 문서를 잘 읽어보고, 요청을 보내기 전에 테스트를 진행하는 것이 좋습니다.

## 참고

- [Apple Developer Documentation - URLRequest](https://developer.apple.com/documentation/foundation/urlrequest)
- [Apple Developer Documentation - URLSession](https://developer.apple.com/documentation/foundation/urlsession)
- [HTTP Methods for RESTful Services](https://www.restapitutorial.com/lessons/httpmethods.html)