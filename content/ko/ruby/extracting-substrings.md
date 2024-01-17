---
title:                "부분 문자열 추출하기"
html_title:           "Ruby: 부분 문자열 추출하기"
simple_title:         "부분 문자열 추출하기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## 무엇 & 어째서?

부분 문자열 추출이란 무엇인지와 프로그래머들이 그것을 왜 하는지를 설명합니다.

## 어떻게:

아래 코드 블록 내에서 코딩 예제와 샘플 출력을 제공합니다.

```Ruby
# 원본 문자열 생성
str = "Hello World"

# 첫 번째 문자부터 세 번째 문자까지 추출
str[0, 3] #=> "Hel"

# 마지막 문자열 두 개 추출
str[-2, 2] #=> "ld"

# 특정 문자열을 기준으로 나누고, 그 중 첫 번째 문자열 추출
str.split(" ")[0] #=> "Hello"
```

## 깊이 파고들기:

1. 히스토리 컨텍스트: 문자열 추출은 고대부터 프로그래밍 언어의 일부였습니다. 그리고 루비에서는 간단하고 강력한 메소드를 제공하여 문자열 추출을 더욱 쉽게 만들었습니다.
2. 대안: 문자열 추출에는 여러 가지 대안이 있지만, 루비는 정규식이나 람다 표현식 등 다른 언어에 비해 더 간결하고 세련된 방식을 제공합니다.
3. 구현 상세: 루비에서는 내부적으로 문자열을 배열처럼 취급하고, 인덱스를 이용하여 부분 문자열을 추출합니다. 동시에 사용되는 메소드는 인덱스를 활용한 문자 선택과 문자열 조작을 제공합니다.

## 참고 자료:

관련 소스 링크입니다.

- [루비 가이드](https://www.ruby-lang.org/en/)
- [루비 API 문서](https://ruby-doc.org/)