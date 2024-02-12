---
title:                "기본 인증을 사용한 HTTP 요청 보내기"
aliases:
- /ko/python/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-20T18:02:38.285126-07:00
model:                 gpt-4-1106-preview
simple_title:         "기본 인증을 사용한 HTTP 요청 보내기"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
HTTP 요청에 기본 인증을 사용하는 건 웹 서비스에 로그인 정보를 전송할 때 필요합니다. 프로그래머들은 서버에 보안된 정보 접근 권한을 요청하는데 이 방식을 사용합니다.

## 어떻게 할까요?:
파이썬의 `requests` 모듈로 기본 인증을 사용해 HTTP 요청을 하는 예시 코드입니다:

```Python
import requests
from requests.auth import HTTPBasicAuth

# 귀하의 사용자 이름과 비밀번호로 대체하세요.
username = 'your_username'
password = 'your_password'

# 인증이 필요한 URL.
url = 'https://api.example.com/'

# 기본 인증으로 GET 요청 보내기.
response = requests.get(url, auth=HTTPBasicAuth(username, password))

# 응답 내용 출력
print(response.text)
```

이것은 서버로부터의 응답을 콘솔에 출력합니다.

## 깊이 들여다보기:
기본 인증은 HTTP 프로토콜에서 오래된 방식입니다. UserID와 Password를 Base64로 인코딩하여 `Authorization` 헤더에 넣어 전송합니다. 보안은 SSL/TLS 같은 프로토콜로 강화할 수 있습니다. 대안으로는 OAuth, API 키, 토큰 기반 인증 등이 있습니다. 구현 상세를 더 알고 싶다면 HTTP 표준과 'requests' 모듈 문서를 참고하면 도움이 됩니다.

## 더 알아보기:
- Python `requests` 공식 문서: https://requests.readthedocs.io/
- HTTP 기본 인증 설명: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
- Python `http.client` 모듈 (보다 낮은 수준의 접근을 제공한다면): https://docs.python.org/3/library/http.client.html
- 보안 연결을 위한 SSL과 TLS에 대한 설명: https://www.cloudflare.com/learning/ssl/what-is-ssl/
