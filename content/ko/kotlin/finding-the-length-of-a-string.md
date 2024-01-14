---
title:                "Kotlin: 문자열의 길이 찾기"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# 왜: 문자열의 길이를 찾는 것에 대해 알아볼 이유는 무엇인가요?
문자열의 길이를 알아내는 것은 프로그래밍에서 매우 중요한 요소입니다. 특정 작업을 수행하기 위해 문자열의 길이를 알아야 할 때가 많기 때문에, 이를 알아볼 필요성은 항상 존재합니다. 즉, 어떤 상황에서든 우리는 문자열의 길이를 알아야 합니다.

## 어떻게: Kotlin을 이용한 코드 예제와 출력
문자열의 길이를 알아내기 위해서는 먼저 문자열 변수를 만들어야 합니다. 그리고 `length` 메소드를 사용하여 문자열의 길이를 알 수 있습니다. 아래의 예제를 참고해보세요.

```Kotlin
val str = "안녕하세요"
println(str.length)
```

위의 코드를 실행하면 다음과 같은 출력 결과를 얻을 수 있습니다.

```
5
```

위의 예제와 같이 `length` 메소드를 사용하면 간단하게 문자열의 길이를 알아낼 수 있습니다. 이제 여러분도 문자열의 길이를 알아내는 방법을 익혔습니다.

## 깊게 들어가보기: 문자열 길이를 알아내는 더 많은 방법
앞서 언급한 예제는 가장 간단한 방법이었습니다. 하지만 Kotlin에는 `length` 메소드 외에도 더 많은 방법이 있습니다.

`length` 메소드는 문자열에 포함된 모든 문자의 개수를 반환합니다. 따라서 해당 문자열에 공백이나 특수문자가 포함된 경우, 이 역시 길이에 포함됩니다. 이를 제외하고 싶은 경우에는 `trim()` 메소드를 이용하여 공백을 제거한 다음, `length` 메소드를 사용하면 원하는 결과를 얻을 수 있습니다.

또한, Kotlin의 `count()` 메소드를 사용하면 특정 문자 또는 문자열의 개수를 찾을 수도 있습니다. 이를 이용하면 문자열 내에서 특정 문자의 개수를 쉽게 세어볼 수 있습니다.

## 더 알아보기
보다 자세한 내용은 Kotlin 공식 문서를 참고해보세요.

* [코틀린 공식 문서](https://kotlinlang.org/docs/strings.html)
* [Kotlin `length` 메소드 관련 정보](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/length.html)
* [Kotlin `count` 메소드 관련 정보](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.collections/count.html)

# 관련 링크
* [코틀린 문자열 다루기 - 문자열의 길이 알아내기](https://mindorks.com/kotlin/string-in-kotlin-find-length)
* [Kotlin에서 문자열 다루기](https://d2.naver.com/helloworld/4911107)