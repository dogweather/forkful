---
title:                "부분 문자열 추출"
html_title:           "Haskell: 부분 문자열 추출"
simple_title:         "부분 문자열 추출"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
문자열에서 일부분을 추출하는 것을 컴퓨터 프로그래머들이 하는 이유는 무엇일까요? 그것은 우리가 문자열을 조작하고 원하는 정보를 얻기 위해서입니다. 예를 들어, 만약 내 이름이 "홍길동"이라면, "홍"과 "동"이라는 문자열을 추출해 다른 용도로 사용할 수 있습니다.

## 어떻게:
Haskell에서 substring을 추출하는 방법은 매우 쉽습니다. ```take``` 함수를 사용하여 문자열의 원하는 부분을 추출할 수 있습니다. 예를 들어:

```
-- "John Doe" 문자열에서 "John"만 추출
take 4 "John Doe"

-- 결과: "John"
```

아래는 더 복잡한 예시입니다. 문자열 "Hello World"에서 "World"만 추출하는 방법을 보여줍니다.

```
-- 문자열을 공백 문자 기준으로 나누어, 두 번째 요소만 추출
last (tail (words "Hello World"))

-- 결과: "World"
```

## 깊은 탐구:
substring 추출의 역사적 배경은 그리 오래되지는 않았지만, 많은 언어들이 이 기능을 내장하고 있습니다. 따라서 다른 프로그래밍 언어에서도 쉽게 구현할 수 있지만, Haskell에서의 구현은 더욱 간결하고 효율적입니다.

추출한 부분을 다른 용도로 사용할 때, 문자열의 길이나 인덱스를 조작하는데 유의해야 합니다. 이러한 정보를 잘못 다룰 경우, 프로그램이 예상치 못한 결과를 보일 수 있습니다.

## 참고 자료:
- [Haskell 공식 문서](https://www.haskell.org/documentation/#books)
- [Haskell 활용법](https://www.ohaskell.guide/)
- [Haskell Cookbook](http://haskell.tailcalled.com/)