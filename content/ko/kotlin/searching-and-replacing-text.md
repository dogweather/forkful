---
title:                "텍스트 검색과 치환"
html_title:           "Kotlin: 텍스트 검색과 치환"
simple_title:         "텍스트 검색과 치환"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 왜

왜 우리는 텍스트를 검색하고 바꾸는 작업에 흥미를 갖게 될까요? 이 작업을 통해 우리는 코드를 보다 간편하고 효율적으로 작성할 수 있습니다.

## 어떻게

우리는 Kotlin의 ```replace()``` 함수를 사용하여 텍스트를 검색하고 바꿀 수 있습니다. 이 함수는 첫 번째 매개변수로 찾고자 하는 텍스트, 두 번째 매개변수로 대체할 텍스트를 입력받습니다. 아래는 예시 코드와 출력입니다.

```Kotlin
val sentence = "나는 회색 셔츠를 가지고 있어."
val newSentence = sentence.replace("회색", "파랑")

println(newSentence)   // 나는 파랑 셔츠를 가지고 있어.
```

이렇듯 ```replace()``` 함수를 이용하면 간단하게 특정 텍스트를 다른 텍스트로 대체할 수 있습니다.

## 심층 분석

이제 좀 더 깊이 들어가보겠습니다. ```replace()``` 함수는 실제로 문자열 내부의 패턴을 찾아 대체합니다. 만약 특정 텍스트가 여러 번 반복되는 경우, 모두 간단하게 대체할 수 있습니다. 또한 이 함수는 원본 문자열을 변경하지 않고 새로운 문자열을 반환합니다. 이는 우리가 원본 문자열을 유지한 채로 특정 패턴을 한 번에 모두 대체할 수 있다는 장점이 있습니다.

## 더 알아보기

- [Official Kotlin Documentation on Strings](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [Kotlin String Cheat Sheet](https://www.educative.io/blog/kotlin-strings-cheat-sheet)

## 참고

- [Kotlin | Android Developers](https://developer.android.com/kotlin)
- [Kotlin Tutorials | Baeldung](https://www.baeldung.com/kotlin)