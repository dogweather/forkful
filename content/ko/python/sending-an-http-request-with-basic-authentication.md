---
title:                "기본 인증과 함께 http 요청 보내기"
html_title:           "Python: 기본 인증과 함께 http 요청 보내기"
simple_title:         "기본 인증과 함께 http 요청 보내기"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 무엇인가요? 왜하는 걸까요?
주요 인증으로 HTTP 요청을 보내는 것은 프로그래머들이 네트워크 요청을 할 때 보안을 유지하기 위해 사용하는 방법입니다. 이를 통해 사용자 이름과 비밀번호로 인증을 받아 서버에 접근할 수 있습니다.

## 어떻게 하나요?
```Python
import requests

url = 'http://example.com'
username = 'username'
password = 'password'

response = requests.get(url, auth=(username, password))

print(response.status_code)
print(response.content)
```

위 코드에서는 기본 인증(authentication)을 위해 `requests` 라이브러리를 사용했습니다. `auth` 파라미터에 사용자 이름과 비밀번호를 전달하여 서버에 인증을 요청하고, 서버로부터 받은 응답을 출력합니다.

## 깊이 파고들기
먼저, HTTP(하이퍼텍스트 전송 프로토콜)는 인터넷에서 데이터를 주고받는 데 사용되는 프로토콜입니다. 인터넷에서 데이터를 주고받을 때, 서버는 보안을 위해 인증을 요구하고, 이때 기본 인증을 사용할 수 있습니다. 기본 인증은 클라이언트가 서버에 사용자 이름과 비밀번호를 보내 사용자를 인증받는 방식입니다.

## 관련 자료
- [Requests 라이브러리 공식 문서](https://docs.python-requests.org/en/latest/)
- [HTTP(In English)](https://developer.mozilla.org/en-US/docs/Web/HTTP)
- [HTTP 기본 인증(In English)](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#basic_authentication_scheme)