---
title:                "HTTP 요청 보내기"
html_title:           "Clojure: HTTP 요청 보내기"
simple_title:         "HTTP 요청 보내기"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 무엇이고 왜?

HTTP 요청 전송은 웹 서버와 클라이언트 간의 정보 교환을 위한 방법입니다. 프로그래머들은 이를 사용하여 웹 API를 실행하고, 웹 구성 요소의 상태를 제어하거나, 웹 사이트의 데이터를 가져옵니다.

## 어떻게 하나요:

Python에서 HTTP 요청을 보내는 가장 간단한 방법은 `requests` 라이브러리를 사용하는 것입니다. 이 라이브러리에는 GET, POST 및 DELETE와 같은 일반적인 HTTP 메소드가 제공됩니다.

```Python
import requests

# GET 요청
response = requests.get('http://example.com')
# 응답 내용 출력
print(response.text)
```

## 깊게 알아보기

이전에는 소켓 프로그래밍으로 HTTP 요청을 구현했지만, 이는 복잡하고 시간이 많이 소요되었습니다. 이후 `httplib`이 도입되었으나, 여전히 편리하지 않았습니다. `requests` 라이브러리의 등장으로 HTTP 요청 전송이 훨씬 쉬워졌습니다.

최근에는 `aiohttp`와 같은 비동기 HTTP 라이브러리가 인기를 얻고 있습니다. 이 라이브러리들은 동시에 여러 HTTP 요청을 보낼 수 있으므로 효율적인 프로그래밍이 가능합니다.

하지만, HTTP 요청 전송에는 `requests`와 `aiohttp` 외에도 많은 방법이 있습니다. 예를 들어, `http.client`와 `urllib.request`는 Python의 표준 라이브러리에 포함되어 있으며 커스텀화가 가능한 고급 기능을 제공합니다.

## 관련 자료

- Python requests 공식 문서: http://docs.python-requests.org/
- aiohttp 공식 문서: https://aiohttp.readthedocs.io/
- Python http.client 문서: https://docs.python.org/3/library/http.client.html
- Python urllib.request 문서: https://docs.python.org/3/library/urllib.request.html