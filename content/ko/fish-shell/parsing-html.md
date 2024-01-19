---
title:                "HTML 파싱"
html_title:           "Fish Shell: HTML 파싱"
simple_title:         "HTML 파싱"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/parsing-html.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 사용할까?
HTML 파싱은 웹페이지에서 데이터를 추출하기 위해, HTML 코드를 분석하는 것입니다. 프로그래머는 종종 웹스크레이핑이나, 웹크롤링 등의 상황에서 HTML 파싱을 사용합니다.

## 이렇게 사용해보세요:
HTML 파싱을 해보는 간단한 Fish Shell 코드를 살펴보죠.
```Fish Shell
set -l html_data "<p>안녕하세요, Fish Shell 사용자여!</p>"
set -l start (string match -r -i -l "<p>" -- $html_data)
set -l end (string match -r -i -l "</p>" -- $html_data)
echo (string sub --start=$start --length=$end  -- $html_data)
```
출력결과:
```Fish Shell
안녕하세요, Fish Shell 사용자여!
```

## 깊게 들어가보기
HTML 파싱은 웹 스크레이핑의 핵심 요소로, 웹 데이터를 수집하는 데 필수입니다. Fish Shell 외에도 Python의 BeautifulSoup, PHP의 DOMDocument 등 HTML 파싱 라이브러리가 있습니다. HTML 파싱을 할 때는 복잡한 HTML 구조, 에러 페이지, 다양한 문자 인코딩 등 여러가지 변수들을 고려해야 합니다.

## 참고 자료
Fish Shell 공식문서: [https://fishshell.com/docs/current/](https://fishshell.com/docs/current/)
Fish Shell HTML 파싱 가이드: [https://fishshell.com/docs/current/commands.html#string](https://fishshell.com/docs/current/commands.html#string)
BeautifulSoup 가이드: [https://www.crummy.com/software/BeautifulSoup/bs4/doc/](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)
DOMDocument 가이드: [https://www.php.net/manual/kr/class.domdocument.php](https://www.php.net/manual/kr/class.domdocument.php)