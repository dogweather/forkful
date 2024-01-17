---
title:                "기본 인증으로 http 요청 보내기"
html_title:           "Swift: 기본 인증으로 http 요청 보내기"
simple_title:         "기본 인증으로 http 요청 보내기"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 무엇인가요? 
HTTP 기본 인증으로 HTTP 요청을 보내는 것은 무엇인지, 프로그래머들이 왜 이렇게 하는지에 대해 설명하는 두-세 문장입니다.

HTTP 기본 인증은 웹 요청을 할 때 사용하는 보안 방식 중 하나입니다. 기본 인증은 사용자 이름과 비밀번호를 인증 서버에 보내고, 유효한 사용자이면 요청을 수락하는 방식입니다. 많은 웹 서비스가 기본 인증을 사용해 사용자 인증을 처리합니다.

## 하는 방법:
```Swift 
let credentials = "\(username):\(password)".base64EncodedString()
let url = URL(string: "https://example.com/api")!
var request = URLRequest(url: url)
request.httpMethod = "GET"
request.setValue("Basic \(credentials)", forHTTPHeaderField: "Authorization")

URLSession.shared.dataTask(with: request) { (data, response, error) in
    guard let data = data else {
        print("Error: \(error?.localizedDescription ?? "unknown")")
        return
    }
    print(String(data: data, encoding: .utf8))
}.resume()
```
이 코드는 동일한 URL에서 GET 요청을 보내는 방법을 보여줍니다. 사용자 이름과 비밀번호를 포함하는 인증 헤더를 추가하기 위해 `URLSession` API를 사용합니다. 응답 받은 데이터를 기본 문자열로 변환하여 콘솔에 출력합니다.

```Swift
// 예상 출력:
Optional("[1, 2, 3]")
```

## 깊게 살펴보기:
1. HTTP 기본 인증은 RFC 2617에 정의되어 있습니다. 해당 문서를 읽어보면 인증 헤더의 구조와 인증 알고리즘이 어떻게 작동하는지 자세히 설명되어 있습니다.
2. HTTP 기본 인증은 보안 수준이 낮아서 중요한 정보를 전송할 때는 사용하지 않는 것이 권장됩니다. 보안 수준이 높은 다른 인증 방식을 사용하는 것이 좋습니다.
3. Swift에서는 `URLCredential` 및 `URLSessionConfiguration` API를 사용하여 기본 인증을 보다 쉽게 구현할 수 있습니다.

## 참고 자료: 
- [RFC 2617](https://tools.ietf.org/html/rfc2617)
- [Apple Developer Documentation on URLCredential](https://developer.apple.com/documentation/foundation/urlcredential)
- [Apple Developer Documentation on URLSessionConfiguration](https://developer.apple.com/documentation/foundation/urlsessionconfiguration)