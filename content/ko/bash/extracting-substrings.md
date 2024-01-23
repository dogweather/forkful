---
title:                "부분 문자열 추출"
date:                  2024-01-20T17:44:59.628206-07:00
model:                 gpt-4-1106-preview
simple_title:         "부분 문자열 추출"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?
서브스트링 추출하기란, 문자열에서 원하는 일부분을 뽑아내는 것입니다. 데이터 파싱, 로그 분석 등 필요한 정보만 취하기 위해 프로그래머들이 자주 사용합니다.

## How to:
```Bash
# 문자열에서 특정 위치의 서브스트링 추출하기
string="Hello, World!"
echo ${string:7:5}  # World

# 문자열 앞에서부터 특정 문자까지 서브스트링 추출하기
echo ${string%%,*}  # Hello

# 문자열 뒤에서부터 특정 문자까지 서브스트링 추출하기
echo ${string##*, }  # World!
```
출력:
```
World
Hello
World!
```

## Deep Dive
리눅스와 유닉스에서 널리 사용되는 Bash는 사용자와 시스템 간 대화형 통신을 제공하는 명령어 인터프리터입니다. 1989년에 Brian Fox에 의해 개발되었으며, 이후 문자열 처리와 서브스트링 추출 기능이 향상되었습니다. `${}` 구문을 사용하면 간편하게 서브스트링을 추출할 수 있으며, 이는 POSIX 표준의 `expr substr` 명령어와 `awk` 또는 `cut` 등의 대안에 비해 매우 빠른 속도로 실행됩니다. 물론, 간단한 작업에서는 눈에 띄는 차이가 없을 수도 있습니다.

## See Also
- Bash 문자열 조작 가이드: https://www.gnu.org/software/bash/manual/
- 리눅스 명령어 튜토리얼: https://tldp.org/LDP/Bash-Beginners-Guide/html/
- 고급 Bash 스크립팅 가이드: https://tldp.org/LDP/abs/html/
