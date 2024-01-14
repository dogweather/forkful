---
title:                "Kotlin: 표준 오류 쓰기"
simple_title:         "표준 오류 쓰기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 왜 작성해야 하나?

표준 오류로 쓰기를 하는 이유는 디버깅과 예외 처리를 위한 중요한 도구입니다.

## 작성하는 방법

`Kotlin` 언어를 사용하여 다음과 같이 표준 오류로 쓰기를 할 수 있습니다.

```Kotlin
fun main(args: Array<String>) {
  System.err.println("이것은 표준 오류 출력입니다.")
}
```

위의 코드를 실행하면 다음과 같이 표준 오류로 내용이 출력됩니다.

```
이것은 표준 오류 출력입니다.
```

## 깊게 들어가보기

표준 출력이나 표준 오류를 사용하는 이유는 프로그램 실행 중에 발생하는 예외와 에러를 캐치하고 처리하기 위해서입니다. 예를 들어, 사용자가 입력한 값이 잘못되었을 때 오류 메시지를 표준 오류로 출력하면 사용자는 이를 확인하고 적절한 조치를 취할 수 있습니다.

또한, 표준 오류를 사용하면 프로그램의 디버깅에도 도움이 됩니다. 프로그램이 실행 중에 어떤 문제가 발생하면 표준 오류로 오류 메시지를 출력하여 어떤 부분에서 문제가 발생했는지 쉽게 파악할 수 있습니다.

## 더 알아보기

`Kotlin` 공식 문서에서 더 많은 정보를 확인할 수 있습니다: [표준 오류로 쓰기](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-input-stream/err.html)

## 관련 링크

- [표준 오류와 표준 출력의 차이점 알아보기](https://www.tutorialspoint.com/Standard-Error-vs-Standard-Output)
- [Kotlin 공식 문서](https://kotlinlang.org/docs/home.html)
- [Kotlin의 입출력 처리](https://www.geeksforgeeks.org/input-output-io-in-kotlin/)
- [표준 오류 처리 예제](https://kotlinlang.org/docs/reference/exceptions.html#try-catch-finally)