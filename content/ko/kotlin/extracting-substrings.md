---
title:    "Kotlin: 부분 문자열 추출"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## 왜

문자열을 추출하는 것은 우리가 코딩을 할 때 자주 사용되는 작업입니다. 이를 통해 우리는 주어진 문자열에서 필요한 정보를 추출하고 실제로 필요한 작업을 수행할 수 있습니다. Kotlin에서는 간단하고 쉽게 문자열을 추출하고 처리할 수 있기 때문에 이 기능을 배우는 것이 매우 중요합니다.

## 하는 법

```Kotlin
// 원본 문자열 만들기
val originalString = "오늘은 Kotlin 프로그래밍을 배우는 날입니다."

// 첫번째 단어 추출하기
val firstWord = originalString.substringBefore(" ")

// 마지막 단어 추출하기
val lastWord = originalString.substringAfterLast(" ")

// 두번째 단어 추출하기
val secondWord = originalString.substringAfter(firstWord).substringBefore(" ")

// 결과 출력하기
println("첫번째 단어: $firstWord")
println("두번째 단어: $secondWord")
println("마지막 단어: $lastWord")
```

출력 결과:
```
첫번째 단어: 오늘은
두번째 단어: Kotlin
마지막 단어: 날입니다.
```
위의 예시에서는 `substringBefore()`, `substringAfter()`, `substringAfterLast()`와 같은 함수를 사용하여 문자열을 추출하고 있습니다. 이 함수들은 인덱스나 길이를 일일이 지정해줄 필요 없이 간단하게 사용할 수 있어 편리합니다.

## 딥 다이브

Kotlin에서는 `substring()` 함수를 사용하여 문자열의 일부분을 추출할 수 있습니다. 이 함수는 매개변수로 시작 인덱스와 마지막 인덱스를 받을 수 있습니다. 또한 `substringBefore()`와 `substringAfter()` 함수를 사용할 때, 매개변수로 정규식을 전달할 수도 있습니다.

## 더 알아보기

- [Kotlin 공식 문서 - 문자열 처리](https://kotl