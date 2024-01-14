---
title:    "Kotlin: 문자열 대문자로 바꾸기"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## 왜
문자열을 대문자로 바꾸는 것에 참여하는 이유는 무엇일까요? 일단 웹 개발이나 데이터 처리와 같은 다양한 분야에서 사용되기 때문입니다.

## 바꾸는 방법
문자열을 대문자로 바꾸는 것은 코틀린에서 아주 간단합니다. 우선 문자열을 `toUpperCase()` 함수를 사용하여 대문자로 바꿀 수 있습니다.

```
Kotlin fun capitalizingString(input: String): String {
    return input.toUpperCase()
}
```

아래는 결과 코드입니다.

```
capitalizingString("hello") #=> "HELLO"
capitalizingString("world") #=> "WORLD"
```

이제 `toUpperCase()` 함수의 내부 코드를 살펴보겠습니다. `toUpperCase()` 함수는 문자열에서 각 문자를 순서대로 돌면서 대문자로 변환한 다음 최종 결과를 반환합니다. 이런 방식으로 우리는 한 줄의 간단한 코드로 문자열을 대문자로 바꿀 수 있습니다.

## 깊게 파고들기
문자열을 대문자로 바꾸는 작업은 프로그래밍에서 매우 일반적으로 사용되는 작업입니다. 다양한 데이터 처리 작업에서 문자열의 경우 대문자를 사용해야 할 때가 많습니다. 예를 들어, 사용자의 이름과 이메일 주소는 항상 대문자로 입력해야 할 수도 있습니다. 따라서 `toUpperCase()` 함수는 문자열을 변환하는 데 아주 유용합니다.

또한 문자열을 대문자로 변환하는 방법에는 여러 가지가 있습니다. 코틀린에서는 `toUpperCase()`의 대안으로 `toUpperCase(Locale.getDefault())`를 사용할 수 있습니다. 이렇게 하면 사용자의 디폴트 로케일에 맞게 문자열이 대문자로 변환됩니다.

## 참고자료
- [코틀린 문자열 처리 가이드](https://kotlinlang.org/docs/strings.html)
- [코틀린 표준 라이브러리 문서 - Strings](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string/index.html#capitalize)