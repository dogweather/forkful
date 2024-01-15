---
title:                "HTTP 요청 보내기"
html_title:           "Python: HTTP 요청 보내기"
simple_title:         "HTTP 요청 보내기"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 왜
네트워크를 통해 데이터를 주고받는 일은 현대의 모든 어플리케이션에서 중요한 역할을 합니다. HTTP 요청을 보내는 것은 웹 서버로부터 정보를 가져오는 가장 일반적인 방법입니다. 즉, 모든 종류의 웹 데이터를 쉽게 가져올 수 있게 해줍니다.

## 하우 투
HTTP 요청은 Python에서 매우 쉽게 보낼 수 있습니다. 아래의 코드 예제를 참고하여 이해해보세요.

```Python
import requests

# GET 요청 보내기
response = requests.get("http://www.example.com")
print(response.text)

# POST 요청 보내기
payload = {'key1': 'value1', 'key2': 'value2'}
response = requests.post("http://www.example.com", data=payload)
print(response.text)
```

위의 예제에서 우리는 `requests` 모듈을 사용하여 간단하게 HTTP 요청을 보내는 방법을 보여주고 있습니다. `get()` 함수는 GET 요청을, `post()` 함수는 POST 요청을 보냅니다. GET 요청은 매개변수로 URL을 받고, POST 요청은 매개변수로 URL과 데이터(payload)를 받습니다. `response.text`는 서버로부터 받은 응답을 나타냅니다.

## 딥 다이브
HTTP 요청은 요청의 종류(GET, POST, PUT 등)와 URL 외에도 많은 파라미터들을 가지고 있습니다. 예를 들어서, 요청 헤더(header)는 요청의 대상 서버에게 추가 정보를 제공하는 역할을 합니다. 이 외에도 Cookie, User-Agent, Accept 등의 파라미터들이 있으며, 이들을 모두 알아두는 것이 요청을 보내는 데에 있어 중요한 부분입니다.

## 씨 알이오
- [Requests Package Official Documentation](https://requests.readthedocs.io/)
- [HTTP 요청과 응답 개념 정리](https://developer.mozilla.org/ko/docs/Web/HTTP/Overview)
- [HTTP 요청 파라미터에 대한 더 자세한 정보](https://www.w3schools.com/tags/ref_httpmethods.asp)