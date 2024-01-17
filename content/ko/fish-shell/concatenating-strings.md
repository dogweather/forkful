---
title:                "문자열 연결하기"
html_title:           "Fish Shell: 문자열 연결하기"
simple_title:         "문자열 연결하기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
문자열 연결이 무엇인지 알아봅시다. 문자열 연결은 문자열을 한 줄로 합치는 것을 의미합니다. 프로그래머는 보통 변수나 함수의 출력 결과를 한 줄로 합쳐서 편리하게 사용하기 위해서 문자열 연결을 하게 됩니다.

## 하는 방법:
아래에 코드 블록 형식으로 코딩 예제와 출력 결과를 보여드리겠습니다. Fish Shell에서는 문자열을 연결하는 방법이 다양합니다. 아래 예제를 참고해보세요.

```Fish Shell
set name "John" # "John"이라는 변수를 정의합니다.
echo "Hello " $name "!" # "Hello John!"이라는 결과가 출력됩니다.
```

## 더 들어가기:
문자열 연결은 프로그래밍에서 매우 중요하며, 개발자들이 자주 사용하는 기능입니다. 예전에는 문자열을 합치기 위해 많은 수작업이 필요했지만, 지금은 문자열 연결 함수나 메소드를 제공하는 다양한 언어들이 있습니다. 또한 문자열 보간이라는 기능을 이용해서도 문자열을 연결할 수 있습니다. Fish Shell에서는 ```string concatenate```라는 내장 함수를 제공하므로, 이를 이용하면 더욱 편리하게 문자열을 연결할 수 있습니다.

## 관련 정보:
Fish Shell 공식 문서에서 문자열 연결에 대한 자세한 내용을 확인하실 수 있습니다. [https://fishshell.com/docs/current/cmds/string.html#concatenate](https://fishshell.com/docs/current/cmds/string.html#concatenate) 또한 다양한 프로그래밍 언어에서 문자열을 연결하는 다른 방법들을 알아보고 싶다면 아래 링크를 참고하세요.
[https://www.w3schools.com/python/python_strings_concatenate.asp](https://www.w3schools.com/python/python_strings_concatenate.asp)