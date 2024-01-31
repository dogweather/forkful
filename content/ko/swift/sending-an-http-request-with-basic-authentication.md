---
title:                "기본 인증을 사용한 HTTP 요청 보내기"
date:                  2024-01-20T18:02:35.622074-07:00
model:                 gpt-4-1106-preview
simple_title:         "기본 인증을 사용한 HTTP 요청 보내기"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why? (무엇인가요? 왜 사용하나요?)
HTTP 요청을 기본 인증과 함께 보내면 사용자 이름과 비밀번호를 전송하여 서버 자원에 접근할 수 있습니다. 이를 통해 개발자들은 보안이 필요한 데이터를 안전하게 교환할 수 있습니다.

## How to: (어떻게 하나요?)
```Swift
import Foundation

// 기본 인증 정보 설정
let username = "user"
let password = "pass"
let loginString = "\(username):\(password)"
guard let loginData = loginString.data(using: String.Encoding.utf8) else { return }
let base64LoginString = loginData.base64EncodedString()

// URL 준비 및 요청 생성
if let url = URL(string: "https://example.com/api/data") {
    var request = URLRequest(url: url)
    request.httpMethod = "GET"
    request.setValue("Basic \(base64LoginString)", forHTTPHeaderField: "Authorization")
    
    // URLSession 사용하여 HTTP 요청 보내기
    let session = URLSession.shared
    let task = session.dataTask(with: request) { data, response, error in
        guard let data = data, error == nil else {
            print(error?.localizedDescription ?? "Unknown error")
            return
        }
        if let httpResponse = response as? HTTPURLResponse, httpResponse.statusCode == 200 {
            // 응답 처리
            if let responseString = String(data: data, encoding: .utf8) {
                print("Response data: \(responseString)")
            }
        }
    }
    task.resume()
}
```
Sample Output:
```
Response data: {"message": "Access granted"}
```

## Deep Dive (깊은 이해)
기본 인증(Basic Authentication)은 HTTP 표준의 일부로, 가장 간단한 형태의 HTTP 사용자 인증입니다. RFC 7617 문서에 정의되어 있죠. 역사적으로 웹 초기에 개발되어서 많은 시스템들과 호환성이 있습니다. 다만, HTTPS와 같은 암호화된 연결 없이 사용할 경우, 크래킹에 취약하다는 단점이 있습니다.

대안으로는 OAuth 또는 JWT(JSON Web Tokens) 같은 더 복잡하지만 안전한 인증 시스템이 있습니다. 이 기법들은 토큰 기반 인증을 사용해 데이터의 무결성과 보안을 강화합니다.

구현 세부사항으로 Swift에서는 URLRequest를 사용하여 HTTP 요청을 만들었습니다. 이후 URLSession을 통해 서버와 비동기 통신을 실행합니다. 기본 인증을 위해 HTTP 헤더에 인코딩된 사용자 이름과 비밀번호를 추가해야 합니다.

## See Also (참고 자료)
- [NSURLRequest Class Reference](https://developer.apple.com/documentation/foundation/nsurlrequest)
- [URLSession Class Reference](https://developer.apple.com/documentation/foundation/urlsession)
- [RFC 7617, The 'Basic' HTTP Authentication Scheme](https://tools.ietf.org/html/rfc7617)
- [Working With JSON in Swift](https://developer.apple.com/swift/blog/?id=37)
