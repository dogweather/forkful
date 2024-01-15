---
title:                "HTML 파싱하기"
html_title:           "Python: HTML 파싱하기"
simple_title:         "HTML 파싱하기"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/parsing-html.md"
---

{{< edit_this_page >}}

## 왜

웹 스크래핑, 데이터 마이닝 및 다른 웹 기반 프로젝트에 참여하기 위해서는 HTML 문서의 구조와 내용을 파악할 수 있어야합니다. 이를 위한 강력한 도구 중 하나가 파이썬의 HTML 파싱입니다.

## 어떻게

**1. HTML 코드 가져오기**
```Python
import urllib.request
response = urllib.request.urlopen('https://www.python.org/')
html = response.read().decode('utf-8')
```

**2. Beautiful Soup 사용하여 파싱하기**
```Python
from bs4 import BeautifulSoup
soup = BeautifulSoup(html, 'html.parser')
```

**3. 원하는 데이터 추출하기**
```Python
# 특정 태그의 내용 가져오기
text = soup.find('h1').get_text()
# 클래스 이름으로 태그의 내용 가져오기
link = soup.find('a', class_='directory').get('href')
# 특정 속성을 포함하는 모든 태그 가져오기
images = soup.find_all('img',{'alt': 'Python Logo'})
# 텍스트 정리하기
clean_text = soup.get_text().strip()
```

코드를 실행하면 다음과 같은 결과를 얻을 수 있습니다.
```Python
Python Screen Session
www.python.org
Dir1
Dir2
Dir3
... 
```

## 딥 다이브

파이썬의 BeautifulSoup 라이브러리는 웹 스크래핑과 관련된 많은 기능을 제공합니다. 특히, HTML의 특정 부분에 쉽게 접근하고 원하는 데이터를 추출하는 데 매우 유용합니다. 또한 CSS 선택자 및 XPath를 사용하여 검색을 더욱 정교하게 할 수 있습니다.

## 관련 자료

- [Beautiful Soup 공식 문서](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)
- [파이썬으로 HTML 파싱하기](https://www.dataquest.io/blog/web-scraping-tutorial-python/)
- [웹 스크래핑과 파이썬을 통한 데이터 분석 입문](https://medium.com/@kswalawage/a-simple-guide-to-web-scraping-using-python-beautifulsoup-1424bee35aae)