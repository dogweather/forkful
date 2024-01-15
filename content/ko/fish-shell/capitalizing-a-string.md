---
title:                "문자열 대문자로 바꾸기"
html_title:           "Fish Shell: 문자열 대문자로 바꾸기"
simple_title:         "문자열 대문자로 바꾸기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 왜

문자열을 대문자로 바꾸는 것이 왜 유용한지 궁금하신가요? 문자열을 대문자로 바꾸는 것은 사용자 입력을 통일된 형식으로 처리할 때나, 대소문자 구분 없이 일치하는 문자열을 검색할 때 등에 유용합니다.

## 코딩 방법

우선, Fish Shell을 설치해야 합니다. 그런 다음, 아래에 제시된 코드를 사용하여 문자열을 대문자로 바꿀 수 있습니다.

```
Fish Shell에서 대문자로 바꾸기:

echo "hello world" | tr a-z A-Z

Fish Shell에서 모든 글자 소문자로 바꾸기:

set -L mystring "HELLO WORLD"
echo $mystring | tr A-Z a-z
```

위의 코드를 실행하면 "HELLO WORLD"라는 문자열이 "hello world"로 변환됩니다. 또한, 마지막 예제에서는 모두 소문자로 변환된 문자열이 "mystring"이라는 변수에 저장되어 출력됩니다.

## 더 깊이 생각해보기

위의 예시는 간단한 문자열을 대/소문자로 변환하는 방법을 보여주기 위한 것입니다. 하지만, 실제로 Fish Shell에서 문자열을 다루는 데에는 더 많은 기능들이 있습니다. 예를 들어, 다음 명령어를 사용하면 문자열의 특정 부분만 대문자로 바꿀 수 있습니다.

```
tr '[:lower:]' '[:upper:]' <<< "hello world" | sed 's/O/o/g'
```

위의 코드를 실행하면 "hellO wOrld"라는 결과를 얻을 수 있습니다. 즉, 문자열을 특정 규칙에 따라 변환하거나 추출하는 방법을 배울 수 있습니다.

## 관련 링크

- [Fish Shell 공식 홈페이지](https://fishshell.com/)
- [Fish Shell 설치하기](https://fishshell.com/docs/current/setup.html)
- [Fish Shell GitHub 저장소](https://github.com/fish-shell/fish-shell)