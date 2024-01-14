---
title:    "Kotlin: 표준 오류 작성하기"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 왜
표준 오류에 쓰기에 참여하는 이유는 무엇일까요? 오류 메시지를 기록하는 것은 프로그래밍 과정에서 발생할 수 있는 에러를 추적하고 디버깅하는 데에 매우 유용합니다.

## 하는 법
```Kotlin
fun main() {
    val num = 10
    val result = num / 0 // Dividing by 0 will cause an error
    System.err.println(result)
}
```

위의 예시 코드는 디버깅을 위해 0으로 나누는 간단한 연산을 수행하고 있습니다. 결과는 `Infinity`입니다. 그러나 이 코드를 실행하면 오류 메시지가 표준 오류 출력으로 출력됩니다. 이를 통해 프로그래머가 프로그램이 오류를 발생시키고 있다는 것을 알 수 있습니다.

## 심층 분석
표준 오류 출력은 콘솔 창에 오류 메시지를 표시하는 데 사용되는 출력 스트림입니다. 이를 이용하여 에러를 추적하고 디버깅하는 것이 표준 방법이며, 코틀린에서는 `System.err`을 이용하여 접근할 수 있습니다. 또한 오류 메시지는 일반적으로 빨간색으로 강조되어, 사용자가 쉽게 구별할 수 있도록 도와줍니다.

## 관련 링크들
- [코틀린 공식 문서](https://kotlinlang.org/docs/reference/)
- [코틀린으로 콘솔 애플리케이션 만들기](https://www.baeldung.com/kotlin/console-application)
- [표준 출력, 표준 에러 출력 이해하기](https://jeong-pro.tistory.com/130)