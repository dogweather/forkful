---
title:                "HTML 파싱"
aliases: - /ko/python/parsing-html.md
date:                  2024-02-03T19:13:00.850209-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTML 파싱"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?
HTML 파싱은 웹페이지의 HTML 코드를 분석하여 특정 정보나 요소를 추출하는 작업으로, 웹 스크래핑, 데이터 마이닝 또는 웹사이트와의 자동 상호 작용을 위한 일반적인 작업입니다. 프로그래머는 프로그래밍 방식으로 웹사이트와 상호 작용하거나 데이터를 추출하고, 작업을 자동화하거나, 웹 애플리케이션을 테스트하기 위해 이 작업을 합니다.

## 방법:
Python은 BeautifulSoup과 requests 같은 강력한 라이브러리를 제공하여 웹 스크래핑과 HTML 파싱을 할 수 있습니다. 시작하기 전에, 이 라이브러리들을 설치하지 않았다면 설치해야 합니다:

```bash
pip install beautifulsoup4 requests
```

`requests`를 사용하여 웹페이지의 HTML 내용을 가져오고 `BeautifulSoup`로 파싱하는 기본 예제는 다음과 같습니다:

```python
import requests
from bs4 import BeautifulSoup

# 웹페이지의 내용을 가져옵니다
URL = 'https://example.com'
page = requests.get(URL)

# HTML 내용을 파싱합니다
soup = BeautifulSoup(page.content, 'html.parser')

# 웹페이지의 제목을 추출하는 예제
title = soup.find('title').text
print(f'웹페이지 제목: {title}')
```

**샘플 출력**:
```
웹페이지 제목: 예제 도메인
```

웹페이지에서 모든 링크를 추출하는 것과 같은 더 복잡한 쿼리의 경우, 파싱 트리를 탐색하고 검색하는 다양한 방법을 사용할 수 있습니다:

```python
# <a> 태그 내의 모든 링크 추출
links = soup.find_all('a')

for link in links:
    href = link.get('href')
    print(href)
```

**샘플 출력**:
```
https://www.iana.org/domains/example
```

BeautifulSoup의 유연성은 정확히 필요한 데이터를 검색하기 위해 검색을 맞춤 설정할 수 있게 해주어, 웹 콘텐츠를 다루는 프로그래머들에게 HTML 파싱을 강력한 도구로 만듭니다.
