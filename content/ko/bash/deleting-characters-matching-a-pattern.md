---
title:                "패턴과 일치하는 문자 삭제"
html_title:           "Bash: 패턴과 일치하는 문자 삭제"
simple_title:         "패턴과 일치하는 문자 삭제"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
패턴과 일치하는 문자를 삭제하는 것은 쉘 스크립트에서 종종 사용되는 기술입니다. 예를 들어, 파일 이름에서 특정 문자를 제거하거나, 문자열에서 특정 패턴을 가진 부분을 삭제할 수 있습니다. 프로그래머는 이를 사용하는 이유로는 문자열을 정제하거나 코드를 간결하게 유지하기 위해서입니다.

## 사용 방법:
```Bash
# 파일 이름에서 .txt 확장자 제거하기
filename="sample.txt"
new_filename="${filename%.*}"
echo "${new_filename}" # sample

# 문자열에서 숫자 문자 제거하기
string="abc123"
new_string="${string//[0-9]}" 
echo "${new_string}" # abc
```
 
## 깊게 파헤치기:
1. 패턴과 일치하는 문자를 삭제하는 기술은 grep이나 sed 같은 다른 명령어와 함께 사용될 수 있습니다.
2. 또한, 사용자가 직접 삭제 함수를 정의해서 이를 사용할 수도 있습니다.
3. 이 기능은 Bash 버전 2 이후부터 사용할 수 있습니다.

## 관련 정보:
- [리눅스 명령어 grep 사용법](https://www.clien.net/service/board/cm_linux/45795)
- [리눅스 명령어 sed 사용법](https://www.clien.net/service/board/cm_linux/45886)
- [Bash 버전 업데이트 내역](https://en.wikipedia.org/wiki/Bash_(Unix_shell))