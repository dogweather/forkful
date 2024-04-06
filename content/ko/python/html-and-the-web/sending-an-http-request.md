---
date: 2024-01-20 18:00:44.489022-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) Python\uC5D0\uC11C\
  \ HTTP \uC694\uCCAD\uC744 \uBCF4\uB0B4\uB824\uBA74 `requests` \uB77C\uC774\uBE0C\
  \uB7EC\uB9AC\uB97C \uC0AC\uC6A9\uD558\uB294 \uAC83\uC774 \uC77C\uBC18\uC801\uC785\
  \uB2C8\uB2E4. \uB2E4\uC74C\uC740 \uAC04\uB2E8\uD55C GET \uC694\uCCAD\uC758 \uC608\
  \uC785\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:56.452140-06:00'
model: gpt-4-1106-preview
summary: "(\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) Python\uC5D0\uC11C HTTP \uC694\uCCAD\
  \uC744 \uBCF4\uB0B4\uB824\uBA74 `requests` \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C\
  \ \uC0AC\uC6A9\uD558\uB294 \uAC83\uC774 \uC77C\uBC18\uC801\uC785\uB2C8\uB2E4."
title: "HTTP \uC694\uCCAD \uBCF4\uB0B4\uAE30"
weight: 44
---

## How to: (어떻게 하나요?)
Python에서 HTTP 요청을 보내려면 `requests` 라이브러리를 사용하는 것이 일반적입니다. 다음은 간단한 GET 요청의 예입니다.

```Python
import requests

response = requests.get('https://api.github.com')
print(response.status_code)
print(response.json())
```

실행 결과는 다음과 같습니다.

```
200
{'current_user_url': 'https://api.github.com/user', 'current_user_authorizations_html_url': 'https://github.com/settings/connections/applications{/client_id}', ... }
```

POST 요청을 보내는 예제:

```Python
import requests

data = {'key': 'value'}
response = requests.post('https://httpbin.org/post', data=data)
print(response.status_code)
print(response.json())
```

실행 결과는 다음과 같습니다.

```
200
{'args': {}, 'data': '', 'files': {}, 'form': {'key': 'value'}, ... }
```

## Deep Dive (심층 분석)
HTTP 요청은 웹의 기본적인 동작 중 하나입니다. 1991년에 개발된 HTTP는 타이밍과 컨텍스트에 따라 여러 버전(HTTP/1.0, HTTP/1.1, HTTP/2, 그리고 최근의 HTTP/3)이 있습니다. `requests`는 사용하기 쉽고, 많은 내장 기능이 있어 Python에서 가장 인기 있는 HTTP 클라이언트 라이브러리 중 하나입니다.

대안으로는 `http.client`가 내장되어 있으며, 비동기 처리를 위한 `aiohttp` 같은 라이브러리도 있습니다. 각 라이브러리는 구현 세부 사항과 지원하는 기능면에서 차이가 있어, 프로젝트의 요구에 맞게 선택해야 합니다.

## See Also (함께 보기)
- `requests` 문서: https://requests.readthedocs.io
- Python 공식 `http.client` 문서: https://docs.python.org/3/library/http.client.html
- `aiohttp` 문서: https://docs.aiohttp.org

이 기사를 통해 HTTP 요청에 대한 기본적인 이해를 얻기를 바랍니다. 더 깊이 탐구하고 싶다면 상기 링크된 문서들에서 추가 정보를 찾아볼 수 있습니다.
