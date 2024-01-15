---
title:                "부분 문자열 추출하기"
html_title:           "C: 부분 문자열 추출하기"
simple_title:         "부분 문자열 추출하기"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## 왜

문자열에서 일부분을 추출하는 것은 많은 상황에서 유용합니다. 예를 들어, 특정 단어나 구를 검색하거나 다른 문자열에 일치하는 부분을 찾는 등 다양한 작업에 활용할 수 있습니다.

## 하는 방법

C 언어에서 문자열을 추출하는 방법은 다음과 같습니다:

```C
// 원본 문자열
char str[] = "Hello World";

// 추출할 부분의 시작 인덱스와 길이
int start = 6;
int length = 5;

// 추출된 부분을 저장할 문자열 변수 선언
char substr[length + 1];

// 길이만큼 문자를 복사하여 추출
strncpy(substr, str + start, length);

// 추출된 부분에 NULL 문자 추가
substr[length] = '\0';

// 추출된 부분 출력
printf("추출된 부분: %s", substr);
```

출력:
```
추출된 부분: World
```

## 딥 다이브

위 예시에서는 `strncpy()` 함수를 사용하여 문자열을 추출했습니다. 이 함수는 첫 번째 매개변수로 추출할 문자열을 저장할 변수를 받고, 두 번째 매개변수로 원본 문자열에서 추출할 부분의 시작 인덱스를 받습니다. 마지막 매개변수는 추출할 문자의 개수를 나타냅니다.
하지만 이 함수는 추출한 문자열 마지막에 NULL 문자를 자동으로 추가하지 않으므로, 따로 처리해주어야 합니다. 또한, 원본 문자열과 추출한 문자열이 겹치지 않도록 주의해야 합니다.

추출된 부분의 시작 인덱스나 길이를 변수로 받아 동적으로 추출할 수도 있습니다. 또한, 여러 개의 부분을 추출하여 다른 문자열에 합칠 수도 있으며, 각각의 추출된 부분을 개별적으로 사용할 수도 있습니다.

## 그 밖에 알아볼만한 것들

- [C 문자열 관련 함수 레퍼런스](https://www.tutorialspoint.com/c_standard_library/string_h.htm)
- [C 언어 문자열 기초 강좌](https://modoocode.com/136)
- [문자열에서 특정 단어 찾기 예시](https://www.programiz.com/c-programming/examples/find-substring)