---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:17.170813-07:00
description: "\uC5B4\uB5BB\uAC8C: \uC8FC\uB85C, Fish shell\uC740 \uC9C1\uC811\uC801\
  \uC73C\uB85C HTML\uC744 \uD30C\uC2F1\uD558\uAE30 \uC704\uD574 \uC124\uACC4\uB418\
  \uC9C0 \uC54A\uC558\uC2B5\uB2C8\uB2E4. \uD558\uC9C0\uB9CC, `curl`, `grep`, `sed`,\
  \ `awk` \uAC19\uC740 Unix \uB3C4\uAD6C\uB4E4\uC744 \uD568\uAED8 \uC0AC\uC6A9\uD558\
  \uAC70\uB098, `pup` \uB610\uB294 Python \uC2A4\uD06C\uB9BD\uD2B8\uC5D0\uC11C `beautifulsoup`\
  \ \uAC19\uC740 \uC804\uBB38\u2026"
lastmod: '2024-03-13T22:44:55.852475-06:00'
model: gpt-4-0125-preview
summary: "\uC8FC\uB85C, Fish shell\uC740 \uC9C1\uC811\uC801\uC73C\uB85C HTML\uC744\
  \ \uD30C\uC2F1\uD558\uAE30 \uC704\uD574 \uC124\uACC4\uB418\uC9C0 \uC54A\uC558\uC2B5\
  \uB2C8\uB2E4."
title: "HTML \uD30C\uC2F1"
weight: 43
---

## 어떻게:
주로, Fish shell은 직접적으로 HTML을 파싱하기 위해 설계되지 않았습니다. 하지만, `curl`, `grep`, `sed`, `awk` 같은 Unix 도구들을 함께 사용하거나, `pup` 또는 Python 스크립트에서 `beautifulsoup` 같은 전문 도구를 사용하는데 뛰어납니다. 아래 예제들은 Fish shell 내에서 이러한 도구들을 이용하여 HTML을 파싱하는 방법을 보여줍니다.

### `curl`과 `grep` 사용하기:
HTML 컨텐츠를 가져와 링크가 포함된 줄을 추출합니다:

```fish
curl -s https://example.com | grep -oP '(?<=href=")[^"]*'
```

출력:
```
/page1.html
/page2.html
...
```

### `pup` 사용하기 (HTML을 파싱하기 위한 커맨드라인 도구):
먼저, `pup`이 설치되어 있는지 확인합니다. 그런 다음 태그, id, 클래스 등으로 요소를 추출할 수 있습니다.

```fish
curl -s https://example.com | pup 'a attr{href}'
```

출력은 `grep` 예제와 유사하게, `<a>` 태그의 href 속성을 나열합니다.

### Python 스크립트와 `beautifulsoup` 사용하기:
Fish 자체는 네이티브로 HTML을 파싱할 수 없지만, Python 스크립트와는 원활하게 통합됩니다. 아래는 Python과 `BeautifulSoup`을 사용하여 HTML에서 타이틀을 파싱하고 추출하는 간결한 예제입니다. Python 환경에 `beautifulsoup4`와 `requests`가 설치되어 있는지 확인하세요.

**parse_html.fish**

```fish
function parse_html -a url
    python -c "
import sys
import requests
from bs4 import BeautifulSoup

response = requests.get(sys.argv[1])
soup = BeautifulSoup(response.text, 'html.parser')

titles = soup.find_all('title')

for title in titles:
    print(title.get_text())
" $url
end
```

사용법:

```fish
parse_html 'https://example.com'
```

출력:
```
Example Domain
```

이러한 각각의 방법은 간단한 커맨드라인 텍스트 조작부터 Python 스크립트에서 `beautifulsoup`의 전체 파싱 파워에 이르기까지, 다양한 사용 사례와 복잡성의 스케일을 지원합니다. HTML 구조의 복잡성과 필요에 따라, 당신은 단순한 Unix 파이프라인이나 더 강력한 스크립팅 접근법을 선택할 수 있습니다.
