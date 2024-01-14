---
title:    "Kotlin: 명령 줄 인자 읽기"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 왜

코틀린 프로그래밍을 하고 있는 독자분들이 커맨드 라인 인자를 읽는 프로그래밍 방법에 대해 배우고 싶으실 수 있습니다.

## 어떻게 하나요

커맨드 라인 인자를 읽는 가장 간단한 방법은 `main` 함수의 `args` 매개변수를 사용하는 것입니다. 이 매개변수는 배열로써 프로그램 실행 시 전달된 모든 인자들을 포함하고 있습니다.

```Kotlin
fun main(args: Array<String>) {
    println("전달된 커맨드 라인 인자들:")
    for (arg in args) println(arg)
}
```

이 예제를 실행하면 다음과 같은 결과를 얻을 수 있습니다.

```
$ kotlin Main.kt a b c
전달된 커맨드 라인 인자들:
a
b
c
```

## Deep Dive

커맨드 라인 인자를 읽는 더 깊이 들어가보면, `kotlin.system` 패키지에 있는 `CommandLine` 클래스를 사용할 수 있습니다.

```Kotlin
import kotlin.system.*

fun main(args: Array<String>) {
    val cmd = CommandLine(args)
    val option = cmd.getOptionValue("option")
    
    if (option != null) {
        println("전달된 옵션 값은 $option 입니다.")
    } else {
        println("옵션 값이 전달되지 않았습니다.")
    }
}
```

이 예제는 `kotlin.system` 패키지를 임포트하고, 커맨드 라인 인자들을 `CommandLine` 클래스로 전달한 후, `getOptionValue()` 메소드를 사용하여 전달된 인자 중 옵션 값을 가져오는 방법을 보여줍니다.

## See Also

- [코틀린 공식 문서: 커맨드 라인 인자](https://kotlinlang.org/docs/tutorials/command-line.html)
- [코틀린 예제 코드: 커맨드 라인 인자 읽기](https://play.kotlinlang.org/byExample/01_introduction/09_command_line_arguments)