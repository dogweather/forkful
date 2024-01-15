---
title:                "부분 문자열 추출하기"
html_title:           "Bash: 부분 문자열 추출하기"
simple_title:         "부분 문자열 추출하기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## 왜

당신이 나눠진 부분 문자열을 추출하는 것에 대해 관심이 있는 이유는 무엇일까요? 이것은 우리가 텍스트에서 원하는 정보를 더 쉽게 찾을 수 있도록 도와줍니다.

## 어떻게

```Bash
# 변수에 문자열 저장하기
str="Unix 환경에서 Bash 프로그래밍을 배워보세요!"

# 첫 번째 단어 추출
echo ${str%% *}
# 결과: Unix

# 두 번째 단어 추출
echo ${str#* }
# 결과: 환경에서

# 세 번째 단어 추출
echo ${str##* }
# 결과: Bash 프로그래밍을 배워보세요!
```

## 딥 다이브

이제 우리는 Bash에서 문자열을 추출하는 다양한 방법을 살펴보았습니다. 이와 함께, 우리는 문자열 연산자를 사용하여 특정 단어를 추출하고 고유한 패턴을 사용하여 원하는 정보를 필터링할 수도 있습니다.

## 참고

- [Bash Guide for Beginners](https://tldp.org/LDP/Bash-Beginners-Guide/html/)
- [Bash String Manipulation](https://www.gnu.org/software/bash/manual/html_node/String-Manipulation.html)