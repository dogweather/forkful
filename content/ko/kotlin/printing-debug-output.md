---
title:    "Kotlin: 디버그 출력 출력"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

# 왜 디버그 출력을 활용하는 걸까요?

프로그래밍을 하는 중에는 항상 디버그 과정을 거치게 됩니다. 하지만 디버그를 진행할 때, 우리는 자주 코드에서 무엇이 일어나는지 확인해야 합니다. 이때 디버그 출력을 활용하면 매우 유용합니다. 디버그 출력은 우리가 직접 출력할 메시지를 정의해놓고, 코드의 어느 부분에서 출력할지 결정할 수 있도록 해줍니다.

## 어떻게 디버그 출력을 활용할까요?

Kotlin에서는 디버그 출력을 위해 `println()` 함수를 활용할 수 있습니다. 이 함수는 원하는 메시지를 콘솔에 출력해줍니다. 예를 들어, 우리가 아래와 같은 코드를 작성하면:

```Kotlin
fun main() {
    println("Hello world!")
}
```

출력은 다음과 같이 될 것입니다:

```
Hello world!
```

만약 우리가 변수의 값을 확인하고 싶다면, 다음과 같이 할 수 있습니다:

```Kotlin
fun main() {
    val num = 5
    println("num의 값은 $num 입니다.")
}
```

이 경우, 출력은 다음과 같이 될 것입니다:

```
num의 값은 5 입니다.
```

우리는 또한 `println()` 함수를 여러 번 사용해 여러 변수나 메시지를 출력할 수 있습니다. 이는 디버그를 진행할 때 매우 효과적일 수 있습니다.

## 디버그 출력의 깊은 곳

디버그 출력은 매우 유용하지만, 우리가 다룰 수 있는 옵션은 이것 뿐만이 아닙니다. Kotlin에서는 `Log` 클래스를 통해 다양한 로그 레벨을 설정할 수 있습니다. 이를 활용하면 특정 상황에서 디버그 출력을 더욱 유연하게 다룰 수 있습니다.

아래는 로그 레벨을 설정하는 방법의 예시입니다:

```Kotlin
fun main() {
    Log.d(TAG, "디버그 메시지")
    Log.e(TAG, "에러 메시지")
    Log.w(TAG, "경고 메시지")
}
```

위의 코드를 실행하면, 각각 다른 로그 레벨에 해당하는 메시지가 출력될 것입니다. 이 밖에도 여러 가지 방법으로 로그 레벨을 설정할 수 있습니다.

# 참조해 볼만한 것들

- [Kotlin 공식 문서: Logging](https://kotlinlang.org/docs/reference/logging.html)
- [StackOverflow: Kotlin으로 디버깅하기](https://stackoverflow.com/questions/41771714/debugging-in-kotlin)
- [코틀린 방송국: 디버깅과 로깅](https://kotlin.kr/docs/reference/logging.html)