---
title:                "문자열을 소문자로 변환하기"
html_title:           "Bash: 문자열을 소문자로 변환하기"
simple_title:         "문자열을 소문자로 변환하기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

문자열을 소문자로 변환하는 것은 모든 대문자를 해당하는 소문자로 바꾸는 프로세스입니다. 프로그래머들은 주로 데이터 검색시 대소문자 불일치 문제를 해결하거나 사용자 입력 값을 표준화하기 위해 이를 사용합니다.

## 어떻게 하는 것인가:

Kotlin에서는 `toLowerCase()`라는 내장 함수를 사용하여 문자열을 소문자로 쉽게 변환할 수 있습니다.

```Kotlin
fun main() { 
    val str = "Hello, World!" 
    println(str.toLowerCase()) 
} 
```

위의 코드를 실행하면 아래와 같은 출력을 얻을 수 있습니다.

```
hello, world!
```

In this example, the string "Hello, World!" is converted to lower case using `toLowerCase()` function.

## 더 깊게 살펴보기:

과거에는 대소문자 개념이 흔치 않았지만, 컴퓨터 언어의 발달과 함께 이 제어 도구가 표준화되었습니다. String 클래스의 `toLowerCase()` 메소드는 모든 문자를 소문자로 변환하며, JVM에서 제공하는 Unicode 표준을 따릅니다. 

대안으로, Kotlin 명령형 스타일의 프로그래밍에서는 문자열을 복사한 다음 각 문자를 돌며 대문자일 경우 소문자로 변환하는 방식을 사용할 수도 있습니다.

```Kotlin
fun main() {
    val string = "Hello, World!"
    var result = ""

    for (char in string) {
        result += if (char.isUpperCase()) char.toLowerCase() else char
    }

    println(result)
}
```

그러나 이 방법은 비효율적이며 `toLowerCase()`를 사용하는 것이 더 좋습니다.

## 참고 사항:

문자열 소문자 변환에 대한 추가 정보 및 다른 방법을 알아보려면 아래의 링크들을 참조하세요.

- Kotlin Official Documentation: String.toLowerCase() - https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/to-lower-case.html
- StackOverflow: Alternatives to toLowerCase - https://stackoverflow.com/questions/22550646/alternatives-to-tolowercase-in-kotlin