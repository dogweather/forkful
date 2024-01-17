---
title:                "HTML 파싱"
html_title:           "Python: HTML 파싱"
simple_title:         "HTML 파싱"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/parsing-html.md"
---

{{< edit_this_page >}}

## 파이썬에서 HTML 파싱하기

## What & Why?
HTML 파싱이란 무엇인가요?
HTML 파싱은 웹 페이지에서 정보를 추출하거나 내용을 분석하기 위해 사용되는 프로세스를 말합니다. 이를 통해 우리는 웹 페이지의 텍스트, 링크, 이미지 등을 쉽게 분석하고 추출할 수 있습니다.

왜 프로그래머들은 이를 사용할까요?
웹 스크래핑, 데이터 마이닝, 정보 추출 등과 같은 다양한 목적으로 사용될 수 있기 때문입니다. 예를 들어, 웹 스크래핑을 통해 수많은 웹 페이지에서 가격 정보를 추출할 수 있어서 경쟁 업체의 가격 변동을 쉽게 파악할 수 있습니다.

## How to:
```python
# requests 모듈 import
import requests

# requests를 사용해 웹 페이지 불러오기
res = requests.get("https://www.example.com")

# BeautifulSoup 모듈 import
from bs4 import BeautifulSoup

# BeautifulSoup을 사용해 HTML 파싱하기
soup = BeautifulSoup(res.text, "html.parser")

# CSS Selector를 이용해 원하는 HTML 태그 추출하기
title = soup.select_one("h1")

# 추출한 태그의 내용 출력하기
print(title.text)

# 여러 개의 태그 추출하기
links = soup.select("a")

# 추출한 태그들의 링크 출력하기
for link in links:
    print(link.get("href"))
```

## Deep Dive:
HTML 파싱의 역사적 배경은 어떻게 되나요?
1990년대에 웹 페이지가 등장하면서 전통적인 데이터 추출 방식은 더 이상 사용할 수 없게 되었습니다. 따라서, HTML 파싱 기술이 발전하며 새로운 방식의 데이터 추출이 가능하게 되었습니다.

대안으로 어떤 것들이 있을까요?
HTML 파싱은 많은 대안들이 존재합니다. Beautiful Soup, lxml, PyQuery 등과 같은 파이썬 라이브러리를 사용하는 것도 일반적인 방법입니다. 또한 정규 표현식을 사용해서도 HTML 파싱이 가능합니다.

HTML 파싱의 구현 방법은 어떻게 되나요?
HTML 파싱은 주로 CSS Selector, XPath 등의 기술을 사용합니다. 이러한 기술들을 사용하면 웹 페이지의 특정 부분을 선택하고 추출할 수 있습니다. 또한, 정규 표현식을 사용하여 추출하거나, 검색 엔진과 같은 기술을 이용해서도 구현할 수 있습니다.

## See Also:
- [Python requests](https://requests.readthedocs.io/en/master/)
- [Beautiful Soup](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)
- [lxml](https://lxml.de/)
- [PyQuery](https://pythonhosted.org/pyquery/)
- [정규 표현식](https://docs.python.org/ko/3.9/library/re.html)
- [CSS Selector](https://www.w3schools.com/cssref/css_selectors.asp)
- [XPath](https://www.w3schools.com/xml/xpath_intro.asp)