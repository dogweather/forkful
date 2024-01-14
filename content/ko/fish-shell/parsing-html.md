---
title:                "Fish Shell: HTML 파싱"
simple_title:         "HTML 파싱"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/parsing-html.md"
---

{{< edit_this_page >}}

## 왜

HTML을 파싱하기 위해 누군가가 참여하는 이유는 무엇인가요?

HTML 파싱은 웹 스크레이핑, 웹 크롤링 및 데이터 마이닝과 같은 웹 데이터 추출 작업에 사용됩니다. 이를 통해 웹 사이트에서 원하는 데이터를 추출하고 분석할 수 있습니다. 또한 프로그래밍적으로 웹 페이지를 조작할 수 있습니다. HTML 파싱은 정보를 얻는데 매우 유용한 도구이며 웹 개발 및 데이터 분석 분야에서 필수적인 기술입니다.

## 이 방법을 사용해보세요

먼저, Fish 쉘을 설치해야 합니다. 그런 다음 새 파일을 만들고 다음 코드를 추가하세요.

```Fish Shell
function parse_html
  set -l web_data (curl -s <URL>)
  set -l parsed_data (string match -r '<tag>*(.*)</tag>' $web_data)
  echo $parsed_data
end
```

위 코드에서는 Fish 쉘의 기본 함수인 `string match`을 사용하여 지정된 URL의 HTML에서 특정 태그의 내용을 추출하고 출력합니다. `<tag>`는 원하는 태그로 바꿔주시면 됩니다. 위 함수를 사용하면 해당 URL의 HTML에서 원하는 데이터를 추출할 수 있습니다.

예를 들어, 다음과 같은 HTML 코드가 있다고 가정해봅시다.

```HTML
<div class="wrapper">
  <h1>파싱 연습</h1>
  <p>이것은 예제 문장입니다.</p>
</div>
```

만약 `p` 태그의 내용을 추출하고 싶다면 다음과 같이 함수를 사용하면 됩니다.

```Fish Shell
parse_html <URL> | string match -r '<p>*(.*)</p>'
```

위 코드는 `이것은 예제 문장입니다.`를 출력합니다.

## 깊이 파고들기

HTML 파싱은 종종 웹 데이터 추출 작업에서 사용되지만, 좀 더 깊이 들어가면 많은 것을 할 수 있습니다. Fish 쉘의 `set` 명령어를 사용하여 추출한 데이터를 변수에 저장하고 다양한 방식으로 사용할 수 있습니다. 또한 `string replace`를 사용하여 추출한 데이터에 대한 추가적인 가공 작업을 수행할 수 있습니다. 이를 통해 웹 사이트에서 원하는 데이터를 추출하고 자신의 목적에 맞게 가공할 수 있습니다.

## 더 읽어보기

- [Fish Shell 공식 페이지](https://fishshell.com/)
- [웹 데이터 추출을 위한 파이썬 라이브러리](https://www.crummy.com/software/BeautifulSoup/)
- [웹 스크레이핑과 데이터 마이닝의 차이](https://www.omega-z.com/ko/2019/12/09/web-scraping-vs-data-mining/)
- [파싱에 대한 다양한 가이드와 자료](https://github.com/XXHolic/fish-commands/wiki/Parsing-guide-and-resources)