---
title:                "부분 문자열 추출"
html_title:           "Kotlin: 부분 문자열 추출"
simple_title:         "부분 문자열 추출"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## 왜: 
문자열에서 부분 문자열을 추출하는 이유는 종종 문자열을 조작하거나 검색할 때 필요하기 때문입니다. 예를 들어, 사용자로부터 입력받은 문자열에서 특정한 단어를 찾거나, 문자열을 조합하여 새로운 문자열을 생성할 때 부분 문자열을 추출하는 것이 유용합니다.

## 추출 방법: 
```Kotlin
// 문자열 생성
var str = "Hello, World!"

// 시작 인덱스와 끝 인덱스를 지정하여 부분 문자열 추출
var subStr = str.substring(0, 5)

// 출력: Hello
println(subStr)
```
부분 문자열을 추출하기 위해서는 `substring()` 함수를 사용합니다. 이 함수는 시작 인덱스와 끝 인덱스를 지정하여 원하는 범위의 문자열을 추출할 수 있습니다. 위 코드에서는 `Hello, World!`라는 문자열에서 인덱스 0부터 5 이전까지의 범위인 `Hello`를 추출하고 출력합니다.

## 깊이 파헤치기:
문자열에서 부분 문자열을 추출하는 데에는 여러 가지 방법이 있습니다. 예를 들어, `substring()` 함수를 사용하는 것 외에도 `slice()` 함수를 사용하여 특정 인덱스의 문자를 추출하거나, 정규식을 이용하여 패턴에 맞는 부분 문자열을 추출하는 등의 방법이 있습니다. 또한, 추출한 부분 문자열을 수정하거나 다른 타입으로 변환하는 것도 가능합니다.

## 이와 관련된 글:
- [Kotlin 공식 문서 - String Manipulation](https://kotlinlang.org/docs/basic-syntax.html#string-manipulation)
- [Kotlin 문자열 다루기](https://academy.realm.io/kr/posts/try-kotlin-strings/)