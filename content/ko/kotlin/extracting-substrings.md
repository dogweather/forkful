---
title:                "문자열 추출하기."
html_title:           "Kotlin: 문자열 추출하기."
simple_title:         "문자열 추출하기."
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

# 무엇이고 왜?

문자열에서 일부 부분을 추출하는 것을 의미합니다. 프로그래머들이 왜 이것을 하는지는 문자열을 특정 부분만 사용하고 싶을 때가 있기 때문입니다.

# 어떻게:

```kotlin
val str = "Hello World"
val subStr = str.substring(0, 5)
println(subStr)
// output: "Hello"
```

```kotlin
val str = "abc123"
val subStr = str.substring(3, 6)
println(subStr)
// output: "123"
```

# 심층 탐구:

1. 문자열 추출은 오래된 소프트웨어 개발 기술 중 하나입니다. 이전에는 수작업으로 문자열에서 특정 부분을 찾아서 사용했지만, 이제는 코딩을 통해 더 간단하고 효율적으로 사용할 수 있습니다.
2. 다른 대안으로는 정규식을 사용하는 것이 있습니다. 정규식을 사용하면 훨씬 더 복잡한 규칙에 따라 문자열에서 특정 부분을 추출할 수 있습니다.
3. 문자열에서 부분을 추출하는 방법에는 여러 가지가 있지만, 코틀린에서 제공하는 substring() 함수가 가장 간단하고 사용하기 쉽습니다. 이 함수는 시작 인덱스와 끝 인덱스를 매개변수로 받아서 해당 부분을 추출합니다.

# 참고 자료:

- 코틀린 공식 문서 https://kotlinlang.org/docs/reference/basic-types.html#strings
- 정규식에 대한 자세한 정보 https://regexone.com/