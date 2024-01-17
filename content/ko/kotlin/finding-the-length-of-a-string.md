---
title:                "문자열 길이 찾기"
html_title:           "Kotlin: 문자열 길이 찾기"
simple_title:         "문자열 길이 찾기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

문자열의 길이를 찾는 것이 무엇인지와 왜 개발자들이 그것을 하는지에 대해 간략하게 설명하겠습니다.

첫째로, 문자열의 길이란 단순히 문자열에서 사용된 문자의 개수를 의미합니다. 예를 들어, "Hello"라는 문자열은 5개의 문자를 포함하고 있으므로 길이는 5가 됩니다.

둘째로, 개발자들은 주어진 문자열의 길이를 알고 싶을 수 있습니다. 이는 문자열의 길이가 다른 연산 및 출력과 관련이 있는 경우가 있기 때문입니다.

## 어떻게:

아래에 고정된 길이로 된 문자열을 찾는 예시와, 사용자가 입력한 문자열의 길이를 찾는 예시를 제공하겠습니다.

```
Kotlin val exampleString = "This is a fixed length string"
println("The length of this string is: ${exampleString.length}")
```

Output: The length of this string is: 30

```
Kotlin print("Enter a string: ")
val userInput = readLine()
println("The length of this string is: ${userInput.length}")
```

Output:

Enter a string: Hello, World!
The length of this string is: 13

## 깊게 파헤치기:

1) 문자열의 길이를 찾는 것은 일반적으로 문자열을 처리하는 언어나 프로그래밍 환경에서 중요한 역할을 합니다. 예를 들어, 입력받은 정보의 길이를 알아야만 정확하게 처리할 수 있기 때문입니다.

2) 문자열의 길이를 찾는 데에는 다양한 방법이 있습니다. 예를 들어, index를 사용하여 문자열의 각 문자를 반복적으로 검색하면서 길이를 찾는 방법도 있고, 또는 내장된 함수를 사용하는 방법도 있습니다. 이러한 다양한 방법을 알고 있다면 더욱 효율적으로 문자열의 길이를 찾을 수 있습니다.

3) Kotlin에서는 문자열을 다루기 위한 내장 함수인 "length"를 제공하며, 이를 사용하여 간단하고 빠르게 문자열의 길이를 찾을 수 있습니다. 또한, 문자열의 길이를 다루는 것과 관련된 다른 함수들도 제공하고 있습니다.

## 관련 자료:

- Kotlin 공식 문서: https://kotlinlang.org/docs/reference/basic-types.html#strings
- GeeksforGeeks의 "Kotlin | String length() Method" 문서: https://www.geeksforgeeks.org/kotlin-string-length-method/