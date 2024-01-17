---
title:                "웹 페이지 다운로드하기"
html_title:           "Python: 웹 페이지 다운로드하기"
simple_title:         "웹 페이지 다운로드하기"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

웹 페이지 다운로드란 무엇인가요?: 
웹 페이지 다운로드란 웹 사이트에서 컴퓨터로 파일을 다운로드하는 것입니다.

프로그래머들이 웹 페이지를 다운로드하는 이유는 무엇일까요?:
웹 페이지 다운로드는 웹 사이트에서 정보를 가지고 오는데에 사용되며, 이 정보는 데이터 분석, 스크래핑, 또는 소프트웨어 개발에 활용될 수 있습니다. 프로그래머들은 웹 페이지를 다운로드하여 이러한 정보를 사용할 수 있게 됩니다.

## 어떻게:

웹 페이지를 다운로드하는 방법은 다양합니다. 
여기서는 파이썬의 라이브러리 중 하나인 `requests`를 사용하여 쉽고 빠르게 웹 페이지를 다운로드하는 방법을 알아보겠습니다.

```Python
import requests
r = requests.get('https://www.example.com')
print(r.text)
```

위의 코드는 예제 웹 사이트에서 웹 페이지를 다운로드하고, 다운로드한 페이지의 내용을 출력합니다.

```Python
import requests
r = requests.get('https://www.example.com')
with open('example.html', 'w') as f:
    f.write(r.text)
```

위의 코드는 다운로드한 웹 페이지를 파일로 저장하는 예제입니다.

## 깊이 알아보기:

웹 페이지 다운로드는 웹의 발전과 함께 발전해왔습니다. 
과거에는 텍스트 기반의 웹 페이지만 다운로드할 수 있었지만, 현재는 다양한 형식의 웹 페이지를 다운로드할 수 있습니다. 
또한, requests 외에도 `urllib`이나 `urllib2`를 사용하여 웹 페이지를 다운로드할 수 있습니다. 
이러한 라이브러리들은 다양한 기능을 제공하므로, 자신에게 맞는 방식으로 사용할 수 있습니다.

## 참고 자료:

웹 페이지 다운로드에 대한 자세한 정보는 다음 링크를 참고하세요.

- requests library: https://requests.readthedocs.io/
- urllib library: https://docs.python.org/3/library/urllib.html
- urllib2 library: https://docs.python.org/2/library/urllib2.html