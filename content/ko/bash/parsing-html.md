---
title:                "HTML 파싱"
html_title:           "Bash: HTML 파싱"
simple_title:         "HTML 파싱"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/parsing-html.md"
---

{{< edit_this_page >}}

## 무엇인가 & 왜?
HTML 파싱은 HTML 문서를 분석하여 원하는 정보를 추출하는 것을 의미합니다. 프로그래머들은 이를 하는 이유는 웹 스크래핑이나 데이터 마이닝과 같은 작업을 수행하기 위해서입니다.

## 방법:
```Bash
# 웹페이지에서 h1 태그의 텍스트 추출하기
curl "https://www.example.com" | grep "<h1>" | sed -e "s/<[^>]*>//g"
```
```Bash
# 웹 페이지에서 특정 클래스가 지정된 모든 링크 추출하기
curl "https://www.example.com" | grep "class=\"myLink\"" | sed -e "s/^.*href\s*=\s*\"\([^ ]*\)\".*$/\1/g"
``` 

## 깊이 들어가기:
HTML 파싱은 인터넷이 발전하면서 생겨난 기술로, 웹 페이지의 구조와 데이터를 추출하는 데에 사용됩니다. 다른 방법으로는 DOM 파싱이 있는데, 이는 HTML 문서를 구성하는 모든 요소를 객체로 변환하여 조작하는 것입니다. 하지만 Bash에서는 일반적으로 ```grep```과 ```sed```을 사용하여 문자열을 추출하고 조작합니다.

## 참고자료:
- [HTML 파싱을 위한 Bash 스크립트 예제](https://stackoverflow.com/questions/7479445/what-command-do-i-need-to-use-to-extract-particular-keystrings-using-grep/7479478#7479478)
- [Bash에서 문자열 추출하는 방법](https://www.linuxjournal.com/content/bash-string-manipulation)