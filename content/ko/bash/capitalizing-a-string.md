---
title:                "문자열 대문자 만들기"
html_title:           "Bash: 문자열 대문자 만들기"
simple_title:         "문자열 대문자 만들기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 왜

문자열의 첫 글자를 대문자로 바꾸는 것은 프로그래밍에서 자주 사용되는 작업입니다. 이로 인해 문자열이 더 읽기 쉬워지고, 프로그램의 사용성이 향상됩니다.

## 하는 법

우선, 우리는 문자열의 첫 번째 글자를 대문자로 바꾸는 방법에 대해 알아보겠습니다. Bash 쉘에서는 내장 함수인 `ucfirst`를 사용하면 됩니다. 아래의 코드를 입력해보세요.

```Bash
test_string="hello world"
echo "${test_string^}"
```

위의 예제 코드에서 우리는 `ucfirst` 함수를 사용하여 `^` 기호를 이용해 첫 글자를 대문자로 바꾼 것을 확인할 수 있습니다. 결과는 `Hello world`가 됩니다.

또 다른 방법으로는 `tr` 명령어를 사용하는 것입니다. `tr`은 문자를 다른 문자로 변환해주는 명령어로, 첫 번째 인자로 대문자로 변환하고 싶은 문자를, 두 번째 인자로 변환하고 싶은 문자를 입력하면 됩니다. 아래의 코드를 입력해보세요.

```Bash
test_string="hello world"
echo "$test_string" | tr '[:lower:]' '[:upper:]'
```

위의 예제 코드에서는 `tr` 명령어를 사용하여 `hello world`를 `HELLO WORLD`로 변환하였습니다.

## 딥 다이브

위에서 소개한 방법은 문자열의 첫 번째 글자만 대문자로 바꾸는 방법입니다. 하지만 만약 모든 단어의 첫 글자를 대문자로 바꾸고 싶다면 어떻게 해야 할까요? 이 경우, `awk` 명령어를 사용하면 됩니다. `awk`는 텍스트를 처리하는 유용한 명령어로, 각 단어의 첫 글자를 대문자로 바꿔주는 `toupper()` 함수를 제공합니다. 아래의 코드를 입력해보세요.

```Bash
test_string="hello world"
echo "$test_string" | awk '{for(i=1;i<=NF;i++)$i=toupper(substr($i,1,1)) substr($i,2)}1'
```

위의 예제 코드에서는 `awk` 명령어를 사용하여 `hello world`를 `Hello World`로 변환하였습니다.

## 더 알아보기

- [Bash String Manipulation Guide](https://www.tutorialkart.com/bash-shell-scripting/bash-string-manipulation/#ucfirst)
- [Bash string uppercase to lower or lower to upper case conversion](https://www.cyberciti.biz/tips/shell-case-conversion-to-lowercase-uppercase.html)
- [Bash Manual - 10.2.2 Parameter Substitution](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html)

## 또 다른 방법

- [How to capitalize the first letter of a string in Bash?](https://stackoverflow.com/questions/2264428/how-to-capitalize-the-first-letter-of-a-string-in-bash) (Stack Overflow)
- [String manipulation in Bash/Shell](https://wiki.bash-hackers.org/syntax/pe#string_manipulation_in_bash) (Bash Hackers Wiki)