---
title:                "문자열 연결하기"
html_title:           "Kotlin: 문자열 연결하기"
simple_title:         "문자열 연결하기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

무엇 & 왜?

코틀린에서, 문자열 연결은 두 개의 문자열을 결합하여 새로운 하나의 문자열을 만드는 작업을 말합니다. 이렇게 하는 이유는 때로는 여러 개의 문자열을 결합해야 할 때가 있기 때문입니다. 예를 들어, 사용자 이름을 받아서 인사말을 만들거나, 여러 개의 파일 경로를 결합하여 하나의 경로를 만들 때 사용할 수 있습니다.

어떻게:

```Kotlin
val firstName = "John"
val lastName = "Smith"
val fullName = firstName + " " + lastName
println(fullName)
```

출력:

```
John Smith
```

딥 다이브:

여러분은 문자열을 연결하기 위해 `+` 연산자를 사용할 수 있지만,이는 매우 비효율적일 수 있습니다. 왜냐하면 매번 문자열을 연결할 때마다 새로운 문자열 객체를 만들기 때문입니다. 따라서 많은 문자열을 연결해야 할 경우 `StringBuilder` 클래스를 사용하는 것이 좋습니다. 이 클래스는 내부적으로 문자열 버퍼를 사용하여 문자열을 연결하므로 메모리를 효율적으로 사용할 수 있습니다.

또한 다른 언어에서는 이 작업을 수행하는 방법이 다를 수 있습니다. 예를 들어, 자바에서는 `String.concat()` 메서드를 사용하여 문자열을 연결할 수 있습니다. 코틀린에서는 이와 같은 메서드가 없기 때문에 `+` 연산자를 사용해야 합니다.

마지막으로, `+` 연산자는 코드를 읽기 쉽고 간결하게 만들어 주기 때문에 많은 개발자가 선호하는 방법입니다.

참고 자료:

코틀린 문서에서 문자열 연산자에 대해 더 자세히 알아보실 수 있습니다. 또한 `StringBuilder` 클래스에 대한 자세한 정보는 코틀린 문서나 자바 문서를 참고하시기 바랍니다.