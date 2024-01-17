---
title:                "문자열 대문자로 변환하기"
html_title:           "Fish Shell: 문자열 대문자로 변환하기"
simple_title:         "문자열 대문자로 변환하기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 무엇일까요? 이게 무슨 소리야?
문자열을 대문자로 바꾸는 것은 무엇일까요? 프로그래머들은 왜 이를 하게 될까요?

## 어떻게 하나요?
Fish Shell에서 문자열을 대문자로 바꾸는 방법을 알아봅시다. 아래 코드 블록을 참고해보세요.

```
set my_string "hello world"
echo $my_string | string toupper
```

출력 결과: HELLO WORLD

## 더 나가보기
일반적으로 모든 문자열을 대문자로 바꾸기 위해서는 어떤 방식이 있는지 알아보겠습니다. 또한 대문자가 아닌 다른 형식으로 문자열을 변환하는 방법도 살펴볼까요? Fish Shell에서 대소문자 변환을 구현하는 방법은 어떤 것이 있을까요?

## 더 찾아보기
대문자로 바꾸기와 관련된 다른 소스를 알고 싶다면 아래 링크를 참고해보세요.
- [Fish Shell 공식 문서](https://fishshell.com/docs/current/cmds/string.html#string-toupper)
- [Stack Overflow: Convert string to upper case in Bash](https://stackoverflow.com/questions/2264428/how-to-convert-a-string-to-upper-case-in-bash)
- [Python 입문자를 위한 배경훈련](https://wikidocs.net/4705)