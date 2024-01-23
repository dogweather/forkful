---
title:                "패턴에 일치하는 문자 삭제"
date:                  2024-01-20T17:41:38.021634-07:00
model:                 gpt-4-1106-preview
simple_title:         "패턴에 일치하는 문자 삭제"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
패턴과 일치하는 문자 삭제하기는 특정 문자 또는 문자열을 찾아서 제거하는 과정입니다. 프로그래머들은 데이터 정제, 입력 형식 맞추기, 또는 필요 없는 정보를 배제할 때 이 작업을 수행합니다.

## How to: (방법)
```Bash
# 문자 제거 예시: 'apple'에서 'p' 제거하기
$ echo "apple" | tr -d 'p'
ale

# 문자열 제거 예시: 'banana-123'에서 숫자 제거하기
$ echo "banana-123" | sed 's/[0-9]//g'
banana-

# 패턴으로 일치하는 전체 문자열 제거하기
$ my_string="Hello World 123"
$ echo ${my_string// *[0-9]*/}
Hello
```
위 코드는 'tr'과 'sed' 명령어, 그리고 Bash 패턴 치환을 사용하여 문자와 문자열을 제거하는 방법을 보여줍니다.

## Deep Dive (심층 분석)
문자 제거 기능은 Unix/Linux의 초기부터 사용되었습니다. 'tr' 명령어는 문자 집합을 변환하거나 삭제하는 데 쓰이며, 'sed'는 텍스트를 처리하는 스트림 편집기로, 복잡한 패턴 매칭과 치환이 가능합니다. Bash 자체에도 문자열 처리 기능이 내장되어 있어 패턴 매칭과 치환을 할 수 있습니다. `tr`은 단순 문자 변환에 적합하고, `sed`는 고급 텍스트 처리에 강력합니다. Bash 패턴 치환은 스크립트 내부에서 빠르게 작업하고자 할 때 유용합니다.

## See Also (더 보기)
- [GNU 'sed' Manual](https://www.gnu.org/software/sed/manual/sed.html)
- [GNU 'tr' Manual](https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html)
- Bash 문자열 조작에 관한 [Advanced Bash-Scripting Guide](https://www.tldp.org/LDP/abs/html/string-manipulation.html)
