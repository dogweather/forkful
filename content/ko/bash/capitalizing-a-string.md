---
title:                "문자열 대문자화"
html_title:           "Bash: 문자열 대문자화"
simple_title:         "문자열 대문자화"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

문자열을 대문자화한다는 것은 모든 글자를 대문자로 바꾸는 과정을 말합니다. 프로그래머들은 가장 대표적으로 사용자 입력을 표준화하기 위해 이런 행동을 취합니다. 

## 어떻게 하는가:

Bash에서 문자열을 대문자로 바꾸는 간단한 방법입니다.

```Bash
str="hello"
echo ${str^^}
```

이 코드의 출력 값은 HELLO입니다.

## Deep Dive:

## 제대로 파보기

문자열을 대문자로 바꾸는 행위는 오래된 컴퓨터 시스템에서부터 시작되었습니다. 특히 상호 대응성을 유지하며 데이터를 정리하는 필요성 때문에 이 과정이 중요해졌습니다.

Bash에서는 ^^ 연산자를 사용하여 문자열을 대문자로 바꿀 수 있습니다. 하지만 다른 프로그래밍 언어에서는 다르게 구현됩니다. 예를 들어, Python에서는 .upper() 메소드를 사용합니다.

Bash에서 문자열을 대문자로 바꾸는 기능은 내부적으로 ASCII 테이블을 사용하여 소문자를 해당하는 대문자로 매핑합니다. 

## 참고자료:

2. [ASCII테이블](http://www.asciitable.com/)
3. [Python.upper()메소드](https://www.w3schools.com/python/ref_string_upper.asp)