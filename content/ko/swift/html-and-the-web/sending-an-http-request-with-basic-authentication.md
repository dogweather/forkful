---
date: 2024-01-20 18:02:35.622074-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) \uAE30\uBCF8 \uC778\
  \uC99D(Basic Authentication)\uC740 HTTP \uD45C\uC900\uC758 \uC77C\uBD80\uB85C, \uAC00\
  \uC7A5 \uAC04\uB2E8\uD55C \uD615\uD0DC\uC758 HTTP \uC0AC\uC6A9\uC790 \uC778\uC99D\
  \uC785\uB2C8\uB2E4. RFC 7617 \uBB38\uC11C\uC5D0 \uC815\uC758\uB418\uC5B4 \uC788\uC8E0\
  . \uC5ED\uC0AC\uC801\uC73C\uB85C \uC6F9 \uCD08\uAE30\uC5D0 \uAC1C\uBC1C\uB418\uC5B4\
  \uC11C \uB9CE\uC740 \uC2DC\uC2A4\uD15C\uB4E4\uACFC \uD638\uD658\uC131\uC774 \uC788\
  \uC2B5\uB2C8\uB2E4. \uB2E4\uB9CC,\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:09.965528-06:00'
model: gpt-4-1106-preview
summary: "(\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) \uAE30\uBCF8 \uC778\uC99D(Basic\
  \ Authentication)\uC740 HTTP \uD45C\uC900\uC758 \uC77C\uBD80\uB85C, \uAC00\uC7A5\
  \ \uAC04\uB2E8\uD55C \uD615\uD0DC\uC758 HTTP \uC0AC\uC6A9\uC790 \uC778\uC99D\uC785\
  \uB2C8\uB2E4."
title: "\uAE30\uBCF8 \uC778\uC99D\uC744 \uC0AC\uC6A9\uD55C HTTP \uC694\uCCAD \uBCF4\
  \uB0B4\uAE30"
weight: 45
---

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
