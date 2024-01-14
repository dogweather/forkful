---
title:                "Python: HTML 파싱"
simple_title:         "HTML 파싱"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/parsing-html.md"
---

{{< edit_this_page >}}

제목: 파이썬에서 HTML 파싱하는 법

## 왜 파싱?

HTML은 웹 페이지를 디자인하고 구조를 정의하기 위해 사용되는 언어입니다. 그런데 HTML은 단순히 텍스트로 이루어져 있기 때문에, 우리가 직접 읽을 수는 있지만 컴퓨터가 이해하고 가공하기에는 적합하지 않습니다. 이 때문에 우리는 파이썬과 같은 프로그래밍 언어를 사용해 HTML을 파싱해야 합니다.

## 파싱하는 방법

HTML을 파싱하는 가장 간단한 방법은 파이썬의 내장 모듈인 `html.parser`를 사용하는 것입니다. 이 모듈은 HTML 문서를 파싱하고 DOM 구조를 탐색하는 기능을 제공합니다.

```python
from html.parser import HTMLParser

# HTML 문서를 파싱하는 클래스를 만듭니다.
class MyHTMLParser(HTMLParser):
    
    # <h1> 태그를 만났을 때 처리하는 함수를 정의합니다.
    def handle_starttag(self, tag, attrs):
        if tag == "h1":
            print("<h1> 태그를 발견했습니다.")

# 파서 객체를 생성하고 HTML 문서를 인자로 넘겨줍니다.
parser = MyHTMLParser()
parser.feed("<html><body><h1>파이썬으로 HTML 파싱</h1></body></html>")

# 출력 결과: <h1> 태그를 발견했습니다.
```

위 코드에서 `handle_starttag()` 함수를 사용해서 원하는 태그를 찾고, 그 태그를 기반으로 원하는 작업을 수행할 수 있습니다.

## 깊게 파헤쳐보기

HTML은 매우 복잡한 구조를 가지고 있기 때문에, 파싱에는 많은 세부 사항이 필요합니다. 파이썬에서는 `BeautifulSoup` 라이브러리를 사용해서 좀 더 쉽게 HTML을 파싱할 수 있습니다.

```python
from bs4 import BeautifulSoup

# HTML 문서를 파싱합니다.
soup = BeautifulSoup("<html><body><h1>파이썬으로 HTML 파싱</h1></body></html>", 'html.parser')

# <h1> 태그를 가진 요소를 찾습니다.
h1_tag = soup.find('h1')

# <h1> 태그의 텍스트를 출력합니다.
print(h1_tag.text)

# 출력 결과: 파이썬으로 HTML 파싱
```

위 코드에서는 `BeautifulSoup` 라이브러리를 사용해서 간단히 `<h1>` 태그를 찾고, 그 안에 있는 텍스트를 출력하는 예시를 보여줍니다. 이 외에도 `BeautifulSoup` 라이브러리는 다양한 기능을 제공하기 때문에, HTML 파싱을 할 때에는 꼭 사용해보시길 추천합니다.

## 참고 자료

- [Python 문서 - `html.parser` 모듈](https://docs.python.org/3/library/html.parser.html)
- [BeautifulSoup 공식 사이트](https://www.crummy.com/software/BeautifulSoup/)
- [Python으로 HTML Parsing하기 - Real Python](https://realpython.com/beautiful-soup-web-scraper-python/)

## 같이 보기

- [BeautifulSoup로 웹 스크래핑하기 - 파이썬 블로그](https://python-bloggers.com/beautifulsoup-web-scraping-with-python/)
- [파이썬 라이브러리 - HTML 파싱하기 - DevInsideYou 블로그](https://devinsideyou.com/2020/04/28/python-library-html-parsing/)