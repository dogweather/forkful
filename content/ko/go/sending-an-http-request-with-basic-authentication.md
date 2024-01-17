---
title:                "기본 인증과 함께 http 요청 보내기"
html_title:           "Go: 기본 인증과 함께 http 요청 보내기"
simple_title:         "기본 인증과 함께 http 요청 보내기"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
- HTTP 기본 인증인 해당하는 HTTP 헤더를 통해 인증 정보를 전송하는 것을 말합니다.
- 개발자들이 하는 이유는 일반적으로 보안상의 이유로 인해 서버가 클라이언트를 인증해야 할 때입니다.

## 방법:
```Go
import "net/http"

// 기본 인증 요청을 보내는 예제
req, _ := http.NewRequest("GET", "http://example.com", nil)
req.SetBasicAuth("username", "password")
res, _ := http.DefaultClient.Do(req)
defer res.Body.Close()

// 인증 정보가 필요한 서버 응답을 받는 예제
body, _ := ioutil.ReadAll(res.Body)
fmt.Println(string(body))
```
- 위의 예제에서, ```username```과 ```password```는 각각 실제 사용자 이름과 비밀번호로 변경해야 합니다.
- 첫 번째 예제에서, ```username```과 ```password```는 HTTP 헤더의 ```Authorization```에 인코딩 된 값으로 설정됩니다.
- 두 번째 예제에서, 서버 응답의 본문을 읽기 위해 ```ioutil``` 패키지를 사용하여 이를 출력합니다.

## 깊게 들어가기:
- HTTP 기본 인증은 1999년에 RFC 2617로 공식화되었습니다.
- 다른 인증 방식으로는 HTTP 다이제스트 인증, OAuth, 토큰 인증 등이 있습니다.
- Go 언어에서는 HTTP 요청을 만드는 데 사용되는 ```http.NewRequest``` 함수를 통해 기본 인증 정보를 설정할 수 있습니다.

## 관련 자료:
- [Go 언어 공식 문서](https://golang.org/pkg/net/http/#Request.SetBasicAuth)
- [RFC 2617](https://tools.ietf.org/html/rfc2617)
- [HTTP 다이제스트 인증에 대한 Go 언어 구현](https://github.com/abbot/go-http-auth)