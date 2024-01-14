---
title:                "Python: HTTP 요청 보내기"
simple_title:         "HTTP 요청 보내기"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 왜

HTTP 요청을 보내는 것이 왜 중요할까요? HTTP 요청은 웹 개발에서 중요한 역할을 합니다. 웹 애플리케이션에서 데이터를 가져오거나 전송하기 위해서는 HTTP 요청을 통해 서버와 통신해야 합니다. 만약 웹 개발에 관심이 있다면, HTTP 요청에 대한 이해는 필수적입니다!

## 어떻게 할까요

파이썬에서 HTTP 요청을 보내기 위해서는 `requests` 라이브러리를 사용합니다. 먼저 라이브러리를 import하고, `get()` 메소드를 사용하여 GET 요청을 보냅니다.

```Python
import requests

r = requests.get('https://www.example.com')
print(r.status_code)
```

위의 코드를 실행하면, `r.status_code`를 통해 서버에서 받은 응답 코드를 확인할 수 있습니다. 또 다른 예제를 살펴보겠습니다.

```Python
payload = {'name': 'Jane', 'age': 25}
r = requests.post('https://www.example.com', data=payload)
print(r.content)
```

위의 코드에서는 `post()` 메소드를 사용하여 POST 요청을 전송하며, `data` 매개변수를 통해 데이터를 전송할 수 있습니다. `r.content`를 이용하면 서버에서 받은 데이터를 확인할 수 있습니다.

## 깊이 파헤치기

HTTP 요청에는 다양한 유형이 있으며, 각각의 유형에 따라 다양한 매개변수를 설정할 수 있습니다. 또한 `requests` 라이브러리 외에도, `urllib`과 같은 내장 라이브러리를 사용하여 HTTP 요청을 전송할 수도 있습니다. 따라서 HTTP 요청에 대한 깊은 이해는 웹 개발에서 매우 중요합니다.

## 더 알아보기

- [requests 라이브러리 공식 문서](https://requests.readthedocs.io)
- [HTTP 요청에 대한 자세한 설명 (MDN)](https://developer.mozilla.org/ko/docs/Web/HTTP/Overview)