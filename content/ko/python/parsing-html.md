---
title:                "HTML 파싱"
html_title:           "Fish Shell: HTML 파싱"
simple_title:         "HTML 파싱"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/parsing-html.md"
---

{{< edit_this_page >}}

## 무엇과 왜?

HTML 파싱이란, 웹 페이지 즉 HTML 문서의 구조를 분석하는 것을 말합니다. 프로그래머들이 이 작업을 수행하려하는 이유는 웹 상에서 데이터를 쉽고 빠르게 가져오기 위함입니다.

## 실습 방법:

Python에서는 `BeautifulSoup` 라이브러리를 이용해 HTML을 쉽게 파싱할 수 있습니다. 간단한 예제를 보겠습니다.

```Python
from bs4 import BeautifulSoup
import requests

# 웹 사이트 HTML 가져오기
response = requests.get('https://www.example.com')
html_content = response.text

# BeautifulSoup 객체 생성
soup = BeautifulSoup(html_content, 'html.parser')

# HTML 파싱
tag = soup.find('h1')
print(tag.text)
```

위 예제의 결과는 웹 사이트 `www.example.com`의 `h1` 태그 내용을 출력하게 될 것입니다.

## 깊이 들어가보기:

HTML 파싱은 웹 스크래핑의 핵심이며, 초기 인터넷 붐의 시기인 1990년대부터 사용되어 왔습니다. Python에서는 `BeautifulSoup` 외에도 `lxml`이나 `html.parser` 등 다른 라이브러리를 이용해 HTML 파싱을 할 수 있습니다.

`BeautifulSoup`는 구조화되지 않은 HTML 및 XML 파일을 구조화하여 파싱하는 데 있어 직관적이고 편리해 넓게 사용되고 있습니다. `lxml`은 속도가 빠르다는 장점이 있지만, `BeautifulSoup`보다 사용하기 어려운 편입니다.

## 추천하는 자료:

아래 자료들을 참조하면 파이썬을 이용한 HTML 파싱에 대해 더 깊게 이해하실 수 있습니다.

1. BeautifulSoup 공식 문서: https://www.crummy.com/software/BeautifulSoup/bs4/doc/
2. lxml 공식 문서: https://lxml.de/
3. Python 공식 문서 - HTML 파싱 모듈: https://docs.python.org/3/library/html.parser.html
4. Real Python - Practical Introduction to Web Scraping in Python: https://realpython.com/python-web-scraping-practical-introduction/