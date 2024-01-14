---
title:                "Kotlin: 문자열 대문자로 변환"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# 왜 대문자로 바꾸는 것이 중요할까요?

문자열을 대문자로 바꾸는 것은 매우 일반적인 작업입니다. 하지만 왜 이 작업이 중요한지 궁금해 하시는 분들도 있을 것입니다. 대문자로 바꿔야 하는 이유를 이해하면 더 효율적인 프로그래밍을 할 수 있을 것입니다.

# 어떻게 하면 문자열을 대문자로 바꿀 수 있을까요?

대문자로 바꾸는 것은 Kotlin 내장 함수인 `toUpperCase()`를 사용하면 간단히 할 수 있습니다. 아래 예제를 참고해보세요.

```Kotlin
val str = "kotlin programming"
println(str.toUpperCase())
```

**출력: KOTLIN PROGRAMMING**

이 외에도 `replace()` 함수를 사용해 문자열 내에서 원하는 부분을 대체하여 대문자로 변경할 수도 있습니다.

```Kotlin
val str = "Kotlin Programming is Fun"
println(str.replace("Fun", "AWESOME").toUpperCase())
```

**출력: KOTLIN PROGRAMMING IS AWESOME**

# 깊게 파고들어보기

문자열을 대문자로 바꾸는 데에는 여러 가지 이유가 있을 수 있습니다. 예를 들어 데이터베이스에 저장된 값들이 모두 대문자로 통일되어 있어서 일관성을 유지해야 할 경우가 있습니다. 또는 사용자로부터 입력받은 문자열을 대문자로 바꾸어 데이터를 처리할 때도 있습니다. 또는 프로그램에서 특정 문자열을 검색하거나 비교할 때 대소문자를 구분하지 않기 위해 문자열을 대문자로 바꿔서 비교하는 것이 좋은 경우가 있습니다.

또한 대문자로 바꾸는 것은 문자열을 깔끔하고 보기 좋게 만드는 데에도 도움이 됩니다. 이는 UI를 개발할 때나 로그를 출력할 때 유용하게 사용될 수 있습니다.

# 더 자세한 정보는 없나요?

더 많은 정보를 원하신다면 Kotlin 공식 문서에서 `toUpperCase()`와 `replace()` 함수를 찾아보세요. 아래 링크를 통해 바로 이동할 수 있습니다.

- `toUpperCase()`: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/to-upper-case.html
- `replace()`: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/replace.html

# 관련 링크

- Kotlin 공식 홈페이지: https://kotlinlang.org/
- Kotlin 문서: https://kotlinlang.org/docs/home.html