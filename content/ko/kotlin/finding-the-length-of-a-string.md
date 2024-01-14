---
title:    "Kotlin: 문자열의 길이 찾기"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## 왜

문자열의 길이를 찾는 것에 대해 고민해 본 적이 있나요? 컴퓨터 과학에서 문자열의 길이를 찾는 것은 매우 흔한 작업입니다. 문자열의 길이를 찾는 것은 프로그램을 작성할 때 필요한 첫 번째 단계입니다. 이 글에서는 Kotlin을 사용하여 문자열의 길이를 찾는 방법에 대해 알아보겠습니다.

## 사용 방법

```Kotlin
fun main() {
    val str = "안녕하세요"
    println(str.length)
}
```
위 코드를 실행하면 출력은 5로 나올 것입니다. 여기서 str은 변수이고, "안녕하세요"는 str 변수의 값입니다. Kotlin에서는 문자열의 길이를 찾기 위해 `.length` 함수를 사용합니다. `str.length`를 작성하면 문자열의 실제 길이를 알 수 있습니다.

또다른 예제를 들어보겠습니다:

```Kotlin
fun main() {
    val str = "Hello, World!"
    println(str.length)
}
```
이 경우 출력은 13이 될 것입니다. 이 예제에서도 `.length` 함수를 사용하여 문자열의 길이를 찾았습니다. 이렇게 간단하게 문자열의 길이를 찾을 수 있다는 것을 알 수 있습니다.

## 딥 다이브

Kotlin에서 `.length` 함수를 사용하여 문자열의 길이를 찾는 것은 매우 간단합니다. 그러나 이 함수는 내부적으로 어떻게 작동할까요? 문자열의 길이를 찾기 위해 `.length` 함수는 실제로 문자열 내부에 있는 모든 문자를 세고 그 수를 반환합니다. 이때 `null` 값이 입력되는 경우 `null` 값 자체의 길이를 반환합니다. 또한 `.length` 함수는 Unicode 값을 기반으로 문자의 길이를 계산하므로 한글도 정확하게 길이를 계산할 수 있습니다.

## 참고

- [Kotlin 공식 문서 - 문자열](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string/index.html)
- [Kotlin Recipes - 문자열 길이 구하기](https://kotlin.recipes/recipes/string-length/)
- [baeldung - Kotlin 문자열](https://www.baeldung.com/kotlin-strings)