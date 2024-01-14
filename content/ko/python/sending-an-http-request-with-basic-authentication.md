---
title:                "Python: 기본 인증을 사용하여 http 요청 보내기"
simple_title:         "기본 인증을 사용하여 http 요청 보내기"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 왜

HTTP 요청을 기본 인증과 함께 보내는 일에 대해서는 왜 그것이 중요한지 궁금할 수 있습니다. 이는 웹 서버와의 상호작용에서 보안을 강화하고 사용자 인증을 통해 더 많은 기능을 활용할 수 있기 때문입니다.

## 하려면

Python에서 HTTP 요청을 기본 인증과 함께 보내는 방법은 간단합니다. 먼저 `requests` 라이브러리를 임포트하고 `auth` 파라미터를 사용하여 인증 정보를 전달합니다.

```python
import requests

auth = requests.auth.HTTPBasicAuth('username', 'password')
r = requests.get('https://example.com', auth=auth)
print(r.status_code)
```

위의 예제에서는 인증 정보를 변수 `auth`에 저장하고, 이를 `auth` 파라미터로 전달하여 `get` 요청을 보냅니다. 이제 해당 URL에 대한 응답을 확인할 수 있습니다.

```
200
```

## 깊이 파보면

HTTP 요청에서 기본 인증은 해당 요청의 특정 도메인 또는 엔드포인트에서만 유효합니다. 즉, 동일한 인증 정보를 사용하여 다른 도메인의 서버에 요청하더라도 인증이 된 상태로 요청을 보낼 수 없습니다.

더 많은 정보를 얻으려면 `requests` 라이브러리의 공식 문서를 참조할 수 있습니다.

## 또 보기

- [Requests: 인터넷 요청을 파이썬 방식으로](https://docs.python-requests.org/en/latest/)
- [HTTP 인증 - 위키백과, 우리 모두의 백과사전](https://ko.wikipedia.org/wiki/HTTP_%EC%9D%B8%EC%A6%9D)