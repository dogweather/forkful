---
title:                "부분 문자열 추출"
html_title:           "Arduino: 부분 문자열 추출"
simple_title:         "부분 문자열 추출"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## 무엇 및 왜?

부분 문자열 추출이란 주어진 문자열에서 특정 부분만 뽑아내는 작업입니다. 이를테면 "안녕하세요, 여러분!"이란 문자열에서 "여러분!"이란 부분을 가져오는 것이죠. 그렇다면 왜 프로그래머들이 이런 작업을 할까요? 그 이유는 문자열 데이터를 효율적으로 분석하고 다루기 위해서입니다.

## 어떻게 해요?

Bash에서 부분 문자열 추출은 아래와 같이 진행됩니다.

```Bash
# Create a string
my_string="안녕하세요, 여러분!"

# Extract a substring
substring=${my_string:10}

# Print the substring
echo $substring
```

이 코드를 실행시키면 아래와 같은 결과를 얻을 수 있습니다.

```Bash
여러분!
```

## 깊이 있게 살펴보기

부분 문자열 추출은 이론적인 개념으로는 1970년대 초반, C 언어의 등장과 함께 시작되었습니다. Bash는 유닉스 쉘 스크립트의 확장 버전으로, 이 기능은 본래 쉘 스크립트에서 상속 받았습니다.

알ternatives에 대해 말하자면, Python이나 Java 같은 다른 언어에서도 부분 문자열 추출이 가능합니다. Bash와 구문은 다르지만 원리는 비슷합니다.

그리고 Bash에서는 `${string:position:length}` 형식을 따릅니다. 'position'은 시작 위치를, 'length'는 길이를 의미합니다. 위치는 0부터 시작합니다.

## 참고 자료

아래 링크에서 더 많은 정보를 얻을 수 있습니다:
1. [Advanced Bash-Scripting Guide: String Manipulations](https://tldp.org/LDP/abs/html/string-manipulation.html)
3. [Python substring guide](https://www.journaldev.com/23584/python-slice-string)