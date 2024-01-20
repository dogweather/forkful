---
title:                "웹 페이지 다운로드하기"
html_title:           "Bash: 웹 페이지 다운로드하기"
simple_title:         "웹 페이지 다운로드하기"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

웹페이지 다운로드란 서버에서 파일을 클라이언트의 컴퓨터로 전송하는 것을 의미합니다. 프로그래머는 이를 통해 웹사이트의 데이터를 분석하거나, 서비스를 개선하는 데 필요한 정보를 모으기 위해서 웹페이지를 다운로드합니다.

## 어떻게 하는가:

Python을 사용하여 웹페이지를 다운로드 해보겠습니다. `requests` 라이브러리를 사용하여 손쉽게 할 수 있습니다.

```Python
import requests

url = "http://www.example.com"
response = requests.get(url)

print(response.text)
```

이 코드는 "http://www.example.com"의 웹페이지를 다운로드하고, 그 내용을 출력합니다.

## 깊이 들어가기:

웹페이지 다운로드는 원래 컴퓨터 네트워크가 등장한 이후로 시작되었으며, 초기에는 FTP(File Transfer Protocol)를 주로 사용하였습니다. 그러나 시간이 지나면서 HTTP가 웹페이지의 표준 데이터 전송 프로토콜로 널리 사용되게 되었습니다.

다운로드를 실행할 때는 라이브러리를 사용하면 편리합니다. Python에서는 `urllib`, `httplib`, `http.client` 등의 라이브러리도 사용할 수 있지만, `requests`는 사용하기 간편하여 가장 널리 사용되는 라이브러리입니다.

## 참고 자료:

더 많은 정보를 얻고 이해를 심화하기 위해 다음의 링크를 참조하시기 바랍니다.
1. Requests: HTTP for Humans - https://requests.readthedocs.io/en/master/
2. HTTP - https://www.w3.org/Protocols/
3. Python’s requests module - https://docs.python-requests.org/en/latest/