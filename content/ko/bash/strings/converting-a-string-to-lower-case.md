---
title:                "문자열을 소문자로 변환하기"
aliases:
- /ko/bash/converting-a-string-to-lower-case/
date:                  2024-01-20T17:37:54.760705-07:00
model:                 gpt-4-1106-preview
simple_title:         "문자열을 소문자로 변환하기"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? (무엇인가 & 왜 사용하는가?)
문자열을 소문자로 변환하는 것은 대문자를 일관된 소문자 형태로 바꾸는 과정입니다. 프로그래머들은 데이터 비교나 정렬할 때 대소문자의 차이를 없애기 위해 사용합니다.

## How to: (어떻게 하나:)
```Bash
# 소문자로 변환하기
my_string="Hello, World!"
lowercase_string=$(echo "$my_string" | tr '[:upper:]' '[:lower:]')

echo $lowercase_string
```
출력:
```
hello, world!
```

```Bash
# Bash 4.0 이상 버전 본인 내장 기능 사용하기
my_string="Hello, World!"
lowercase_string="${my_string,,}"

echo $lowercase_string
```
출력:
```
hello, world!
```

## Deep Dive (심층 분석)
초기 UNIX 시스템에서는 문자열 처리 기능이 기본적이고 제한적이었습니다. `tr` 명령어가 문자를 대체하는 보편적 방법으로 자리 잡았죠. Bash 4.0부터는 문자열 조작이 쉘 자체에서도 가능해져 `tr`을 사용하지 않고도 소문자 변환이 가능합니다. 

`tr`은 간단하고 효율적이나 새로운 문법은 코드를 더 간결하게 만들어줍니다. 예를 들어, `${my_string,,}`는 `$my_string`의 모든 대문자를 소문자로 바꿉니다. 반대로 `${my_string^^}`은 모든 소문자를 대문자로 변환하구요. 

Bash 내장 기능의 장점은 별도의 프로세스 생성 없이 작업을 처리한다는 점입니다. `tr`은 외부 프로그램 호출을 필요로 해서 상대적으로 더 느릴 수 있습니다.

## See Also (더 보기)
- [Bash Reference Manual](https://www.gnu.org/software/bash/manual/bash.html)
- [GNU 'tr' manual](https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html)
- [Advanced Bash-Scripting Guide](https://tldp.org/LDP/abs/html/index.html)
