---
title:                "Haskell: 기본 인증을 사용하여 http 요청 보내기"
simple_title:         "기본 인증을 사용하여 http 요청 보내기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 왜

HTTP 요청을 기본 인증과 함께 보내는 것의 의의는 무엇일까요? 매우 간단합니다. 기본 인증은 보안이 필요한 서비스에 대한 액세스를 보장하기 위한 간단한 방법입니다. 따라서 이를 통해 사용자의 인증을 확인하고 안전한 데이터 교환을 할 수 있습니다.

## 어떻게

Haskell을 이용하여 HTTP 요청에 기본 인증을 추가하는 방법은 매우 간단합니다. 아래의 코드 블록을 참고해주세요.

```Haskell
import Network.HTTP.Simple
import Data.ByteString.UTF8 (fromString)

-- 요청 URL 정의
requestURL = "https://example.com/auth"

-- 인증 정보 생성
authInfo = fromString "username:password"

-- 요청 생성
request = setRequestBasicAuth authInfo requestURL

-- 요청 보내기
response = httpLBS request

-- 결과 출력
print $ getResponseBody response
```

위의 코드는 Network.HTTP.Simple 라이브러리를 이용하여 기본 인증을 포함한 HTTP 요청을 보내는 예시입니다.

## 깊게 파헤쳐보기

HTTP 요청의 헤더에는 기본 인증을 위한 특별한 필드가 포함되어 있습니다. 이 필드에는 사용자의 인증 정보가 암호화되어 포함되어 있으며, 서버는 이를 해석하여 사용자의 인증을 확인하고 요청에 대한 적절한 응답을 반환합니다. 따라서 기본 인증은 미리 암호화된 사용자의 인증 정보를 HTTP 요청에 포함시키는 방법입니다.

## 관련 자료

- [Network.HTTP.Simple 라이브러리 문서](https://hackage.haskell.org/package/http-client-0.7.0/docs/Network-HTTP-Simple.html)
- [HTTP 기본 인증에 대한 자세한 설명](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_authentication_scheme)
- [Haskell에서의 HTTP 요청 보내기](https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/http)