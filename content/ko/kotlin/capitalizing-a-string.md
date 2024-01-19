---
title:                "문자열 대문자화"
html_title:           "Kotlin: 문자열 대문자화"
simple_title:         "문자열 대문자화"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 사용합니까?
문자열 대문자 변환은 모든 단어의 첫 글자를 대문자로 바꾸는 것을 의미합니다. 이를 통해 텍스트를 읽는 게 좀 더 용이해지며 간혹 특정 문법 규칙을 따르는 데도 사용됩니다.

## 어떻게 사용합니까?
다음은 Kotlin에서 문자열의 첫 글자를 대문자로 변환하는 방법입니다.

```Kotlin
fun String.capitalizeWords(): String = split(" ").map { it.capitalize() }.joinToString(" ") 

fun main() {
  val text = "hello world"
  println(text.capitalizeWords())  // 출력: Hello World
}
```
위의 코드는 각 단어의 첫 글자를 대문자로 변환하는 확장 함수를 정의하고 사용하는 예입니다.

## 깊게 알아보기
- **역사 측면**: 문자열 대문자 변환은 프로그래밍 언어가 발명된 초기부터 사용되어 왔습니다. 특히, 더 잘 읽히고 이해하기 쉬운 코드를 작성하는 데 도움이 됩니다.
- **대안**: `capitalize()` 함수 외에도 Kotlin은 첫 글자만 대문자로 바꾸는 `capitalizeFirstLetter()` 같은 다양한 함수를 제공합니다.
- **구현 세부사항**: `capitalize()` 함수는 사실 내부적으로 첫 번째 문자를 대문자로 바꾸고 나머지 문자를 그대로 두는 `replaceFirstChar()` 함수를 사용해 구현됩니다.

## 참고하기
-[Kotlin Official Documentation on Strings](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string/): Kotlin 공식 문서에서 문자열에 대해 자세히 설명하고 있습니다.
-[Kotlin String.capitalize()](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/capitalize.html): 이 함수에 대한 Kotlin 공식 문서 링크입니다.