---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:00.850209-07:00
description: "\uBC29\uBC95: Python\uC740 BeautifulSoup\uACFC requests \uAC19\uC740\
  \ \uAC15\uB825\uD55C \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C \uC81C\uACF5\uD558\uC5EC\
  \ \uC6F9 \uC2A4\uD06C\uB798\uD551\uACFC HTML \uD30C\uC2F1\uC744 \uD560 \uC218 \uC788\
  \uC2B5\uB2C8\uB2E4. \uC2DC\uC791\uD558\uAE30 \uC804\uC5D0, \uC774 \uB77C\uC774\uBE0C\
  \uB7EC\uB9AC\uB4E4\uC744 \uC124\uCE58\uD558\uC9C0 \uC54A\uC558\uB2E4\uBA74 \uC124\
  \uCE58\uD574\uC57C \uD569\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:54.594677-06:00'
model: gpt-4-0125-preview
summary: "Python\uC740 BeautifulSoup\uACFC requests \uAC19\uC740 \uAC15\uB825\uD55C\
  \ \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C \uC81C\uACF5\uD558\uC5EC \uC6F9 \uC2A4\uD06C\
  \uB798\uD551\uACFC HTML \uD30C\uC2F1\uC744 \uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4\
  ."
title: "HTML \uD30C\uC2F1"
weight: 43
---

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
