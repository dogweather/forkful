---
title:                "Kotlin: 문자열 대문자로 바꾸기"
simple_title:         "문자열 대문자로 바꾸기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 왜

문자열을 대문자로 변환하는 것에 참여하는 이유는 무엇일까요? 이 글에서는 Kotlin을 사용하여 문자열을 대문자로 변환하는 방법과 그 깊은 이해를 알아보겠습니다.

## 어떻게

우선, Kotlin에서 제공하는 내장 함수를 사용하여 쉽게 문자열을 대문자로 변환할 수 있습니다.

```
Kotlin val string = "hello world" 
val capitalizedString = string.toUpperCase()
println(capitalizedString)
```

이 코드를 실행하면 "HELLO WORLD"라는 결과가 출력됩니다.

## 깊이 파헤치기

하지만 문자열을 대문자로 변환하는 과정에서 내부적으로 어떤 일이 벌어지는지 알아보겠습니다. 문자열은 내부적으로 문자들의 배열로 표현됩니다. 따라서 대문자로 변환되는 과정에서도 원래의 문자열은 변하지 않고, 새로운 대문자 문자열이 생성됩니다.

이를 더 쉽게 이해하기 위해 코드를 한 줄씩 분해해보겠습니다.

```
Kotlin val string = "hello world" // 입력된 소문자 문자열을 변수에 저장
val characters = string.toCharArray() // 문자열을 문자들의 배열로 변환
for (i in characters.indices) { // 문자 배열의 길이만큼 반복
    val asciiValue = characters[i].toInt() // 각 문자를 아스키 코드 값으로 변환
    if (asciiValue in 97..122) { // 아스키 코드 값이 소문자 범위에 해당하면 (97~122는 소문자 a~z)
        characters[i] = (asciiValue - 32).toChar() // 아스키 코드 값 32를 빼서 대문자로 변환
    }
}
val capitalizedString = String(characters) // 문자 배열을 다시 문자열로 변환
```

이렇게 내부 과정을 살펴보면 대문자로 변환되는 과정이 좀 더 명확하게 이해될 수 있습니다.

## 참고자료

- [Kotlin String 클래스 공식문서](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/) 
- [Kotlin에 대한 오늘의 퀵 트릭: 문자열에서 대문자로 변경](https://medium.com/@krzychukosobudzki/coolest-kotlin-today-picker-string-uppercase-9a8b18eb0511)
- [Java와 Kotlin의 문자열 비교](https://www.baeldung.com/kotlin/java-string-comparison)