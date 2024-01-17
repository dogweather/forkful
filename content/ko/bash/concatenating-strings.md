---
title:                "문자열 연결"
html_title:           "Bash: 문자열 연결"
simple_title:         "문자열 연결"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

# 뭐가/왜 인가요?

문자열을 연결하는 것이 무엇인지, 그리고 프로그래머들이 이것을 왜 하는지에 대해 소개합니다.

## 그럼 어떻게 할까요?

```Bash
var1="Hello"   # 첫 번째 변수를 만듭니다.
var2="World"   # 두 번째 변수를 만듭니다.
concat="${var1} ${var2}"   # 변수를 연결합니다.
echo ${concat}   # 출력: Hello World
```

## 깊게 들어가보면

1. 역사적 배경: 초기의 쉘 프로그래밍에서 문자열 연결은 어려웠습니다. `cat`을 사용하거나 여러 줄의 문자열을 `echo`로 반복적으로 출력하는 등 다양한 방식을 사용했습니다.
2. 대안: 쉘에서 명령의 출력 결과를 변수에 할당할 수 있는 `$(...)`이나 `backticks`을 사용하면, 문자열 연결 뿐만 아니라 다양한 조작을 할 수 있습니다. 또한, AWK, SED 같은 다른 도구를 이용할 수도 있습니다.
3. 구현 세부사항: 변수나 리터럴 값을 `""` 으로 감싸는 것으로 문자열을 연결할 수 있습니다. 여러 변수를 연결할 경우 적절한 공백이 필요하며, 변수가 아닌 리터럴 값을 연결할 때는 이 과정이 필요 없습니다.

## 관련 자료

- [Concatenation of two strings in Shell Scripting](https://www.geeksforgeeks.org/concatenation-of-two-strings-in-shell-scripting/)
- [Bash Guide for Beginners](https://tldp.org/LDP/Bash-Beginners-Guide/html/sect_09_01.html)