---
title:                "기본 인증을 사용하여 http 요청 보내기"
html_title:           "Bash: 기본 인증을 사용하여 http 요청 보내기"
simple_title:         "기본 인증을 사용하여 http 요청 보내기"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 무엇이며 왜?
HTTP 요청을 기본 인증과 함께 보내는 작업은 보안이 필요한 통신에서 사용합니다. 이를 통해 우리는 사용자의 자격 증명을 확인하고, 민감한 정보에 대한 접근을 허용하거나 거부할 수 있습니다.

## 사용법
Swift를 활용해 HTTP 요청을 기본 인증과 함께 보내는 방법을 배워보겠습니다. 다음 예제를 따라해봅시다.

```Swift
import Foundation

let username = "your_username"
let password = "your_password"
let loginString = String(format: "%@:%@", username, password)
let loginData = loginString.data(using: String.Encoding.utf8)!
let base64LoginString = loginData.base64EncodedString()

let url = URL(string: "https://www.your-url.com")!
var request = URLRequest(url: url)
request.httpMethod = "POST"
request.setValue("Basic \(base64LoginString)", forHTTPHeaderField: "Authorization")

let task = URLSession.shared.dataTask(with: request) { (data, response, error) in
    if let error = error {
      print("Error:\n\(error)")
    }
    else if let data = data {
      let str = String(data: data, encoding: String.Encoding.utf8)
      print("Received data:\n\(str ?? "")")
    }
}
task.resume()
```

## 딥다이브
이 기능은 웹이 거대한 플랫폼으로 성장하면서 사람들이 민감한 정보를 쉽게 공유할 수 있는 방법이 필요했기에 나온 것입니다. HTTP Basic Authentication은 그 중 하나일 뿐, 전체 인증 방식의 일부입니다. 대안으로는 OAuth, JWT(Jason Web Token) 등의 더 안전하고 복잡한 인증 방식이 있습니다. 

Swift에서의 HTTP 요청과 기본 인증은 주로 HTTP 헤더를 통해 수행되며 이 작업은 URLRequest의 `setValue(_:forHTTPHeaderField:)` 메소드를 사용하여 구현됩니다. 

## 참고자료
- [Swift.org - URLSession](https://developer.apple.com/documentation/foundation/urlsession)
- [MDN Web Docs - HTTP Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [RFC 7617 - HTTP Authentication: Basic and Digest Access Authentication](https://tools.ietf.org/html/rfc7617)