---
title:                "대소문자로 변환하는 방법"
html_title:           "Bash: 대소문자로 변환하는 방법"
simple_title:         "대소문자로 변환하는 방법"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 왜

문자열을 소문자로 변환하는 것은 많은 경우에 유용한 작업입니다. 예를 들어, 데이터베이스 쿼리를 실행하기 전에 대소문자를 일치시키기 위해 사용할 수 있습니다. 또는 사용자 입력을 검증하거나 파일 이름을 일관된 형식으로 변경하는 등의 용도로 사용할 수 있습니다.

## 방법

우리는 `tr` 명령어를 사용하여 문자열을 소문자로 변환할 수 있습니다. 아래 예제를 참고해주세요.

```Bash
# 변수 선언
my_string="Hello World"

# 소문자로 변환
new_string="$(echo $my_string | tr '[:upper:]' '[:lower:]')"

# 결과 출력
echo $new_string
# output: hello world
``` 

우리는 `tr` 명령어를 이용해 대소문자를 변환했습니다. 이 명령어는 첫 번째 인자로 변환할 대상 문자열을 받고, 두 번째 인자로 변환할 문자열의 대상을 받습니다. 즉, `[:upper:]` 문자들을 `[:lower:]`로 변환하는 것을 의미합니다.

## 깊이 들어가기

때로는 소문자로 변환된 문자열을 변수에 다시 저장하는 것이 중요합니다. 왜냐하면 한 번 소문자로 변환된 문자열은 오류를 유발할 수 있는 파일 이름이나 쿼리문 등에서 사용되기 때문입니다. 따라서 `tr` 명령어는 중요한 유틸리티인 것입니다.

그리고 우리가 그렇게 이용한 `[:upper:]`와 `[:lower:]`는 대문자와 소문자를 의미하는 것 뿐만 아니라, 다른 유니코드 문자에도 적용됩니다. 예를 들어 영어 이외의 언어, 혹은 숫자와 기호에도 적용될 수 있는 유용한 명령어입니다.

## 참고 자료

- [Bash 쉘 스크립팅 가이드](https://wiki.kldp.org/HOWTO/html/Adv-Bash-Scr-HOWTO/index.html)
- [Bash가 기본 셸인 이유](https://www.gnu.org/software/bash/manual/html_node/Introduction.html)
- [Bash 쉘 설명서](https://www.gnu.org/software/bash/manual/html_node/index.html)