---
title:                "Swift: 기본 인증을 사용하여 http 요청 보내기"
simple_title:         "기본 인증을 사용하여 http 요청 보내기"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 왜 HTTP 요청에 기본 인증을 사용하는가?
HTTP 요청은 웹 서버에서 데이터를 가져오는 중요한 방법입니다. 기본 인증은 보안을 강화하기 위해 요청을 보내는 사용자를 인증하는 데 사용됩니다.

## 어떻게 하면 HTTP 요청에 기본 인증을 보낼 수 있을까?
기본 인증을 사용하여 HTTP 요청을 보내는 방법은 간단합니다. 먼저 URL을 생성하고 해당 URL을 `URL` 객체로 변환해야 합니다. 그런 다음 생성한 URL을 사용하여 `URLRequest` 객체를 만들고 메소드를 `GET`으로 설정합니다. 그 후, `HTTPBody`를 설정하여 요청에 필요한 데이터를 추가하고, `HTTPMethod`를 `POST`로 설정하여 요청을 보냅니다.

```Swift
// URL을 생성하고 이를 URL 객체로 변환합니다.
let urlString = "http://www.example.com"
let url = URL(string: urlString)

// URLRequest 객체를 생성하고 메소드를 GET으로 설정합니다.
var request = URLRequest(url: url!)
request.httpMethod = "GET"

// Basic 인증 헤더를 생성하여 요청에 추가합니다.
let username = "username"
let password = "password"
let loginString = String(format: "%@:%@", username, password)
let loginData = loginString.data(using: .utf8)
let base64LoginString = loginData?.base64EncodedString()
request.setValue("Basic \(base64LoginString)", forHTTPHeaderField: "Authorization")

// HTTP 요청을 보냅니다.
let task = URLSession.shared.dataTask(with: request) { (data, response, error) in
    // 요청이 성공하면 데이터를 받아옵니다.
    if let data = data {
        let dataString = String(data: data, encoding: .utf8)
        print(dataString)
    }
}
task.resume()
```

**출력 예시:**

```Swift
Optional("This is the response from the server.")
```

## 깊게 파보아보기
기본 인증은 사용자를 인증하기 위해 사용자 이름과 비밀번호를 인코딩하는 방식으로 작동합니다. 요청을 보내기 전에 사용자 이름과 비밀번호를 Base64로 인코딩한 후 헤더에 추가하면 서버에서는 해당 헤더를 읽어서 사용자를 인증합니다. 이는 보안 위험성이 있기 때문에 HTTPS와 같은 보안 연결을 사용하는 것이 좋습니다.

## 같이 보기
- [URL - Apple Developer Documentation](https://developer.apple.com/documentation/foundation/url)
- [URLRequest - Apple Developer Documentation](https://developer.apple.com/documentation/foundation/urlrequest)
- [앱 개발을 위한 Swift 기초 강좌 - 프로그래머스](https://programmers.co.kr/learn/courses/4)