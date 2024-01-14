---
title:    "Kotlin: 문자열의 길이 찾기"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# 왜: 문자열의 길이를 찾는 것에 참여할 이유

문자열의 길이를 찾는 것은 프로그래밍에서 매우 일반적이고 필수적인 작업입니다. 이 길이를 알면 문자열을 조작하고 처리하는 데 많은 도움이 될 수 있습니다. 따라서 Kotlin의 문자열 길이를 찾는 방법을 배우는 것은 매우 중요합니다.

## 어떻게: 코딩 예제 및 샘플 출력 코드블록 내에서 " ```Kotlin ... ```"을 사용하여 설명합니다.

먼저, `length` 메서드를 사용하여 문자열의 길이를 찾는 방법을 알아보겠습니다.

```
Kotlin
val str = "안녕하세요"
println(str.length)
```

위의 코드의 출력 결과는 `5`가 됩니다. `length` 메서드는 문자열에 포함된 글자 수를 반환합니다. 또한 아래의 예제와 같이 공백과 같은 특수 문자도 포함하여 길이를 반환합니다.

```
Kotlin
val str = "Hello World!"
println(str.length)
```

위의 코드의 출력 결과는 `12`가 됩니다. 이와 같이 `length` 메서드는 문자열에서 실제로 표시되는 모든 문자의 수를 반환합니다.

## 깊게 파고들기: 문자열 길이를 찾는 더 깊은 정보

문자열의 길이를 찾는 방법을 이미 살펴보았으므로 이제는 이와 관련된 몇 가지 유용한 정보를 더 살펴보겠습니다.

첫째, Kotlin에서는 한글이 포함된 문자열의 길이를 정확하게 계산합니다. 이는 한글이나 다른 언어의 문자를 UTF-8 인코딩으로 처리하기 때문입니다. 따라서 문자열 길이를 계산할 때 이 점을 유의해야 합니다.

둘째, 문자열의 길이는 변수 `length`에 저장할 수도 있습니다. 이를 통해 나중에 다른 연산에 사용할 수 있습니다.

예를 들어:

```
Kotlin
val greeting = "안녕하세요"
val numLetters = greeting.length

println(numLetters + 2)
```

위의 코드의 출력 결과는 `7`이 됩니다. 따라서 문자열의 길이를 변수에 저장하면 나중에 이 변수를 사용하여 다른 연산을 수행할 수 있습니다.

## 또 다른 정보 확인하기

이 문서에서는 Kotlin에서 문자열 길이를 찾는 방법을 설명했습니다. 그러나 Kotlin에는 더 많은 문자열 관련 메서드가 있으니 참고하시기 바랍니다.

## 관련 자료

- [Kotlin 문자열 문서](https://kotlinlang.org/docs/basic-types.html#strings)
- [Kotlin 문자열 메서드 참고 가이드](https://www.tutorialspoint.com/kotlin/kotlin_strings.htm)
- [코딩 공부에 도움이 되는 구글 개발자 채널](https://www.youtube.com/channel/UCV9EjLmPIC6VhTUMYgVhTCA)