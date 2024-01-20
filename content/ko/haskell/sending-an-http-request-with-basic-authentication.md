---
title:                "기본 인증을 이용한 HTTP 요청 보내기"
html_title:           "Arduino: 기본 인증을 이용한 HTTP 요청 보내기"
simple_title:         "기본 인증을 이용한 HTTP 요청 보내기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 무엇이며 왜필요한가?
기본 인증이 포함된 HTTP 요청 전송은 서버에 보안성이 요구되는 정보를 전달하는 도구입니다. 프로그래머는 이를 통해 시스템 보안에 기여하며, 인증된 사용자만 선택된 자원에 액세스하도록 할 수 있습니다.

## 어떻게 하는가:
Haskell에서는 "http-client" 및 "http-conduit" 라이브러리를 사용하여 HTTP 요청을 전송합니다. 기본 인증을 위해 "applyBasicAuth" 함수를 사용합니다. 아래에 기본 인증과 함께 GET요청을 전송하는 코드 예시를 보여주겠습니다.

```Haskell
import Network.HTTP.Simple
import Network.HTTP.Client (applyBasicAuth)

getWithBasicAuth :: IO ()
getWithBasicAuth= do
    req <- parseRequest "http://example.com"
    let req' = applyBasicAuth "username" "password" req
    response <- httpLBS req'
    print $ getResponseStatus response
```
위 코드를 실행하면, 인증 후 서버의 응답 상태 코드가 출력됩니다.

## 깊이 있는 이해:
기본 인증은 웹의 초기 단계에서 시작되었습니다. 이 기술은 사용자 이름과 비밀번호를 Base64 인코딩하여 "Authorization" 헤더에 추가하는 방식으로 동작합니다. 단순함에도 불구하고, 네트워크상에서 패킷을 가로챌 수 있는 위험 때문에 이미 SSL/TLS와 같은 보안 계층과 함께 사용되는 것이 일반적입니다.

Haskell의 "http-client"와 "http-conduit"는 HTTP 통신작업을 쉽게 해주는 라이브러리입니다. "applyBasicAuth"는 이를 더욱 쉽게 돕는 함수로, 인증처리를 위해 요청에 필요한 헤더를 적절히 추가해줍니다. 대안으로는 JWT, OAuth가 있는데, 이들은 각각 JSON 기반의 열람 권한 발급 및 사이트 간 권한 부여를 위한 개방형 표준입니다.

## 관련 자료:
Haskell에 대한 추가 학습이 필요하다면 다음 자료를 참고하십시오.

[Haskell 공식 사이트](https://www.haskell.org/)

[http-conduit Github](https://github.com/snoyberg/http-client)

[인증에 대한 Mozilla 개발자 네트워크 문서](https://developer.mozilla.org/ko/docs/Web/HTTP/Authentication)