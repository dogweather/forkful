---
title:    "Kotlin: 문자열을 소문자로 변환하기"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

# 왜

문자열을 소문자로 변환하는 것이 중요한 이유는 프로그래밍에서 일반적인 작업 중 하나이기 때문입니다.

# 어떻게

```Kotlin
fun main() {
    val str = "HELLO WORLD"
    val lowerCaseStr = str.toLowerCase()
    println(lowerCaseStr)
}

//Output: hello world
```

문자열을 소문자로 변환하는 것은 매우 간단합니다. Kotlin에서는 String 클래스에 내장된 toLowerCase() 함수를 사용하면 됩니다. 위의 예시 코드에서는 "HELLO WORLD" 문자열을 소문자로 변환하여 출력하는 것을 볼 수 있습니다. 

# 심층탐구

문자열을 소문자로 변환하는 것은 소문자와 대문자를 구분하지 않는 경우에 매우 유용합니다. 이는 사용자의 입력에 대한 일관성을 유지하기 위해 매우 중요합니다. 또한 문자열을 정렬하거나 검색할 때 대소문자를 구분하지 않으면 코드 작성이 훨씬 더 간단해집니다. 또한, 문자열의 일부분만 소문자로 변환하거나, 모든 문자가 대문자인지 확인할 수도 있습니다. 

# 더 많은 정보

- Kotlin String 클래스 문서: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/index.html
- Kotlin 문자열 다루기: https://kotlinlang.org/docs/strings.html
- Kotlin String 표현식: https://www.baeldung.com/kotlin/string-expression