---
title:                "HTML 파싱"
date:                  2024-01-20T15:30:21.615255-07:00
html_title:           "Arduino: HTML 파싱"
simple_title:         "HTML 파싱"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (무엇이며 왜?)
HTML 파싱이란, HTML 문서에서 구조적인 데이터를 추출하는 과정입니다. 프로그래머들은 웹 스크래핑, 데이터 마이닝, 내용 검증 등의 작업을 위해 HTML을 파싱합니다.

## How to:
Bash에서 HTML을 파싱하는 일반적인 방법은 `grep`, `awk`, `sed`와 같은 텍스트 처리 도구를 사용하는 것이 아니라, `xmllint`나 `pup` 같은 XML/HTML 전용 파서를 사용하는 것입니다. 예제를 통해 설명하겠습니다.

```Bash
# xmllint 설치
sudo apt-get install libxml2-utils

# xmllint를 사용하여 예제 HTML 파일의 제목 추출
xmllint --html --xpath '//title/text()' example.html 2>/dev/null

# Sample Output: "Your Page Title"
```

```Bash
# pup 설치
go get github.com/ericchiang/pup

# pup을 사용하여 예제 HTML 파일에서 모든 링크 텍스트 추출
cat example.html | pup 'a text{}'

# Sample Output:
# Link 1 text
# Link 2 text
# Link 3 text
```

## Deep Dive (심층 분석)
명령줄에서 HTML 파싱이 자주 필요한 작업은 아니지만, 자동화된 스크립트의 일부로 사용될 때 유용합니다. `xmllint`, `pup`와 같은 도구들은 HTML 구조를 이해하고 올바르게 파싱할 수 있도록 XML/HTML 파싱 라이브러리를 기반으로 합니다.

과거에 Bash 사용자들은 주로 텍스트 처리 도구를 사용해 HTML을 처리했지만, 이 방법은 비효율적이고 오류를 일으키기 쉽습니다. HTML은 계층적이고 복잡한 구조를 가진 마크업 언어이기 때문에, 전용 파서를 사용하는 것이 훨씬 더 신뢰할 수 있는 결과를 가져옵니다.

`xmllint`는 `libxml2` 라이브러리의 유틸리티로, XML과 HTML 파일을 위한 검증 및 파싱 도구입니다. `pup`은 Go 언어로 작성된 도구로, `jq`처럼 HTML 문서를 처리합니다. 더 복잡한 HTML 처리가 필요하면, Python의 `BeautifulSoup`나 JavaScript의 `cheerio` 같은 고급 파싱 라이브러리를 고려해야 할 수도 있습니다.

## See Also (참고 자료)
- [xmllint documentation](http://xmlsoft.org/xmllint.html)
- [pup Github repository](https://github.com/ericchiang/pup)
- [BeautifulSoup documentation](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)
- [cheerio Github repository](https://github.com/cheeriojs/cheerio)

이러한 도구들은 Bash 환경에서 HTML을 다루는 실용적인 방법을 제공합니다. 각 도구에 대해 더 알아보려면 위의 링크들을 참고하세요.
