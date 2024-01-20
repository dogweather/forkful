---
title:                "HTML 파싱"
html_title:           "Arduino: HTML 파싱"
simple_title:         "HTML 파싱"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/parsing-html.md"
---

{{< edit_this_page >}}

## 무엇인가요 & 왜 그래야하나요?
HTML 파싱은 웹페이지의 HTML 내용을 분석하고 원하는 정보를 얻어내는 과정입니다. 이를 통해 프로그래머는 웹페이지의 요소를 다루고 웹스크레이핑, 콘텐츠 분류 등 다양한 작업을 수행할 수 있습니다.

## 어떻게 하는 건가요?:
Bash에서 `lynx`, `awk` 처럼 기본적인 CLI 도구를 사용하여 HTML을 파싱해볼 수 있습니다. 예제 코드는 아래와 같습니다.

```Bash
#!/bin/bash
# 우선, lynx를 사용하여 HTML을 텍스트로 변환합니다.
lynx -dump https://google.com > google.txt

# 그리고 awk를 사용하여 원하는 정보를 추출합니다.
awk '/검색/ {print $0}' google.txt
```
이 스크립트는 https://google.com 웹페이지의 HTML을 파싱하여 '검색'이라는 단어가 포함된 라인을 출력합니다.

## 깊이 알아보기: 
HTML 파싱은 웹의 초기 시대부터 수행되는 중요한 작업입니다. 하지만, 복잡한 HTML 페이지에 대해서는 Bash만으로 처리하기 어려울 수 있습니다. 이런 경우, Python의 BeautifulSoup나, Node.js의 Cheerio 같은 modern 라이브러리를 활용하는 것을 고려해볼 수 있습니다.

또, 기억할 점은 Bash가 해석 가능한 정보만을 추출할 수 있다는 것입니다. JavaScript로 생성되는 동적인 내용은 다룰 수 없습니다. 이를 처리하기 위해서는 Selenium과 같은 웹 드라이버가 필요합니다.

## 참고 자료:
이 주제에 대해 더 알아보려면 아래의 링크를 참고하세요:

1. BeautifulSoup: https://www.crummy.com/software/BeautifulSoup/bs4/doc/
2. Cheerio: https://cheerio.js.org/
3. Selenium: https://www.selenium.dev/documentation/en/