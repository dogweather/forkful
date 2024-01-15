---
title:                "기본 인증을 사용하여 http 요청 보내기"
html_title:           "Python: 기본 인증을 사용하여 http 요청 보내기"
simple_title:         "기본 인증을 사용하여 http 요청 보내기"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 왜

누군가가 HTTP 요청을 기본 인증과 함께 보내려고 할 때 *왜* 이를 해야하는지에 대해 최대 2 문장으로 설명합니다.

## 어떻게

Python을 사용하여 HTTP 요청을 기본 인증과 함께 보내는 방법에 대한 코딩 예제와 출력을 포함한 "```Python ... ```" 코드 블록을 제공합니다.

```Python
import requests
url = 'https://example.com'
headers = {'Authorization': 'Basic c29tZV9hc2RmOnNvbWVfcGFzc3dvcmQ='} #base64 encoded username:password
response = requests.get(url, headers=headers)
print(response.status_code)
print(response.text)
```

이 코드는 "https://example.com"에 대해 기본 인증을 사용하여 GET 요청을 보냅니다. 요청에 대한 응답 상태 코드와 텍스트를 출력합니다.

## 딥 다이브

기본 인증은 HTTP 요청에서 사용자 인증을 처리하는 방법 중 하나입니다. 일반적으로 사용자 이름과 비밀번호를 Base64로 인코딩하고 "Authorization" 헤더에 추가하여 요청을 보냅니다. 이는 요청을 보내는 동안 사용자의 개인정보를 보호하는 데 도움이 됩니다. 그러나 이러한 인증 방식은 암호화되지 않으므로 보안 측면에서는 안전하지 않습니다. 따라서 중요한 정보를 전송하는 경우에는 다른 인증 방식을 사용하는 것이 좋습니다.

## 관련 링크

- [Python requests 라이브러리 공식 문서](https://docs.python-requests.org/en/latest/)
- [HTTP 통신에 대한 초보자 가이드](https://developer.mozilla.org/ko/docs/Web/HTTP/Access_control_CORS)
- [Base64 인코딩에 대한 튜토리얼](https://www.base64decode.org/)