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

## 무엇 & 왜?
HTTP 요청을 보내는 것이란 무엇인지 알아보고, 프로그래머들이 왜 그렇게 하는지 이해해 봅시다.

HTTP (HyperText Transfer Protocol)는 웹 페이지에서 다른 데이터를 받아오기 위해 사용되는 프로토콜입니다. 이 프로토콜을 사용해 데이터를 전송하기 위해서는 우리가 다른 서버에 요청을 보내야 합니다. 따라서 프로그래머들은 HTTP 요청을 보내고 그에 따른 응답을 받아오는 것이 중요합니다.

## 어떻게:
아래의 코드 블록 안에서 코딩 예시와 샘플 출력을 살펴봅시다.

```Python
# requests 라이브러리 import
import requests

# GET 요청 보내기
response = requests.get("https://www.example.com")

# 응답 출력
print(response.text)

# POST 요청 보내기
payload = {'key1': 'value1', 'key2': 'value2'}
response = requests.post("https://www.example.com", data=payload)

# 응답 출력
print(response.status_code)
```

## Deep Dive:
(1) Historical Context: HTTP 요청은 1990년대 초반에 등장한 웹 프로토콜입니다. 원래 HTML 페이지를 전송하는 용도였지만, 현재는 다양한 데이터를 전송하기 위해 사용됩니다. (2) Alternatives: Python에서 HTTP 요청을 보내는 다른 방법으로는 urllib 라이브러리를 사용하는 것도 가능합니다. 하지만 일반적으로 requests 라이브러리를 더 많이 사용합니다. (3) Implementation Details: GET과 POST 요청은 각각 서버로부터 데이터를 받기만 하고, 데이터를 보내지 않을 때 사용됩니다. 하지만 PUT, PATCH 또는 DELETE 요청은 서버로부터 데이터를 받는 것이 아니라 서버의 데이터를 수정 또는 삭제할 때 사용됩니다.

## See Also:
관련된 자료를 참고해 보세요.
- [requests library 공식 문서](https://requests.readthedocs.io/en/latest/)
- [urllib library 공식 문서](https://docs.python.org/3/library/urllib.html)