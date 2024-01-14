---
title:                "Bash: HTML 파싱"
simple_title:         "HTML 파싱"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/parsing-html.md"
---

{{< edit_this_page >}}

## 왜

HTML 파싱을 알아야 할까요?

HTML은 우리가 인터넷 상에서 볼 수 있는 대부분의 웹 페이지의 기본 언어입니다. 따라서, 우리가 어떤 웹사이트의 데이터를 분석하거나 특정 정보를 추출하고자 할 때, 그 데이터를 이해하기 위해서는 HTML 파싱이 필수적입니다.

## 사용 방법

이제 우리가 설명할 Bash의 `wget` 명령어를 사용하여 HTML 페이지를 다운로드하고 파싱하는 방법을 알아보겠습니다.

```Bash
#!/bin/bash

# 원하는 웹사이트의 주소를 `URL` 변수에 저장합니다.
URL="www.example.com"

# `wget` 명령어를 사용하여 해당 URL의 HTML 코드를 다운로드합니다.
wget -q -O - "$URL" | \

# 다운로드한 HTML을 grep을 이용해 `title` 태그 안에 있는 텍스트를 추출합니다.
grep -oP '(?<=<title>).*?(?=</title>)'
```

위의 코드를 실행하면, `www.example.com`의 홈페이지 제목이 출력됩니다. 이처럼 파싱을 활용하면, 원하는 정보를 얻고 원하는 형태로 가공할 수 있습니다.

## 깊이 파헤치기

HTML 파싱에 대해 더 깊이 알아보겠습니다. HTML은 태그들의 트리 구조로 이루어져 있기 때문에, 각 태그마다 다른 정보를 담고 있습니다. 따라서, 우리는 이러한 태그들을 파악하여 원하는 정보를 추출할 수 있습니다.

또한, Bash 외에도 파이썬과 같은 다른 프로그래밍 언어를 이용해 HTML 파싱을 할 수도 있습니다. 이는 개발자의 선호에 따라 다를 수 있습니다. 그리고 HTML 파싱뿐만 아니라, 정규 표현식과 함께 사용하면 더욱 강력한 검색과 추출이 가능합니다.

## 참고자료

이외에도 HTML 파싱에 대해 더 알아보고 싶다면, 아래의 링크들을 참고해보세요.

- [Bash 공식 문서](https://www.gnu.org/software/bash/manual/html_node/)
- [마크다운 언어를 이용한 웹사이트 제작](https://daringfireball.net/projects/markdown/)
- [정규 표현식 관련 정보](https://www.regular-expressions.info/)