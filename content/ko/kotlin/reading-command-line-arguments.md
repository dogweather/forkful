---
title:                "Kotlin: 컴퓨터 프로그래밍에서 명령 줄 인수 읽기"
simple_title:         "컴퓨터 프로그래밍에서 명령 줄 인수 읽기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 왜

커맨드 라인 인자를 읽는 것이 왜 중요한지 알고 싶은 이유가 있나요? 이 블로그 포스트는 코틀린으로 커맨드 라인 인자를 읽는 방법을 알려드리겠습니다.

## 하우 투

코틀린을 사용하여 커맨드 라인 인자를 읽는 방법은 간단합니다. 우선 main() 함수의 파라미터로 args를 추가해야 합니다. 그리고 args[0], args[1], ... 등으로 원하는 인자들을 읽을 수 있습니다. 아래는 이를 구현한 예제 코드와 출력 결과입니다.

```Kotlin
fun main(args: Array<String>) {
    println("첫 번째 인자: " + args[0])
    println("두 번째 인자: " + args[1])
    println("세 번째 인자: " + args[2])
}
```

```Kotlin
첫 번째 인자: kotlin
두 번째 인자: programming
세 번째 인자: blog
```

위 예제는 args 배열의 첫 번째, 두 번째, 세 번째 인자를 순서대로 출력하는 코드입니다. 이렇게 간단하게 커맨드 라인 인자를 읽을 수 있습니다.

## 딥 다이브

커맨드 라인 인자를 읽는 것은 프로그램을 더 유연하게 만드는데 도움이 됩니다. 예를 들어, 사용자가 프로그램을 실행할 때 다양한 옵션을 전달하여 원하는 동작을 설정할 수 있습니다. 또는 여러 개의 인자를 받아서 계산을 수행하는 프로그램을 만들 수도 있습니다. 더 많은 기능과 예제는 아래 “See Also” 항목에서 찾아볼 수 있습니다.

## See Also

- [Kotlin 공식 문서: 커맨드 라인 인자 읽기](https://kotlinlang.org/docs/tutorials/command-line.html)
- [Studytonight: 코틀린 커맨드 라인 인자 사용](https://www.studytonight.com/kotlin/kotlin-command-line-arguments)
- [Baeldung: 코틀린 커맨드 라인 인자 사용하기](https://www.baeldung.com/kotlin/command-line-arguments)