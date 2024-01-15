---
title:                "명령줄 인수 읽기"
html_title:           "Kotlin: 명령줄 인수 읽기"
simple_title:         "명령줄 인수 읽기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 왜

"Command line arguments(명령 줄 인자)"를 읽는 것은 프로그래밍에서 매우 유용한 기술입니다. 이것을 이용하면 프로그램을 실행할 때 사용자로부터 입력값을 받을 수 있으며, 이를 통해 프로그램의 실행 방식을 유연하게 조절할 수 있습니다.

## 방법

```Kotlin
fun main(args: Array<String>) {
    for (arg in args) {
        println(arg)
    }
}
```

위의 코드는 "main" 함수의 괄호 안에 "args"라는 변수를 넣어서 실행하면, 사용자가 프로그램 실행 시 프로그램 이름 뒤에 입력한 모든 값을 출력하는 간단한 예제입니다.

**커맨드 라인에서 실행:**

```Kotlin
$ kotlin Main.kt Hello World!
Hello
World!
```

만약 프로그램에서 특정한 인자를 사용하는 것이 아니라면, "전역 변수(global variable)"인 "args"를 사용하는 것도 가능합니다. 하지만 이 방식은 가독성이 떨어져 추천하지 않습니다.

### 옵션 파싱

커맨드 라인 인자는 다양한 형태로 지정할 수 있습니다. 예를 들어, "압축 파일을 만들기 위한 프로그램에서 "-c" 옵션으로 압축 파일을 지정하고 싶을 때, 다음과 같이 사용할 수 있습니다.

```Kotlin
fun main(args: Array<String>) {
    val options = args.filter { it[0] == '-' }
    val files = args.filter { it[0] != '-' }
    println("Options: $options")
    println("Files: $files")
}
```

**옵션과 파일 지정:**

```Kotlin
$ kotlin Main.kt -c output.txt input1.txt input2.txt
Options: [-c]
Files: [output.txt, input1.txt, input2.txt]
```

옵션은 "-c output.txt"처럼 사용할 수 있지만, 파일명이 "-"로 시작하는 경우 처리하기가 어렵기 때문에 "-c=output.txt"와 같이 사용하는 것을 권장합니다.

## 딥 다이브

커맨드 라인 인자는 문자열 배열(Array<String>)로 전달되며, 배열의 크기는 인자 개수에 따라 결정됩니다. 이 배열을 이용하면 프로그램에서 사용자의 입력 값을 쉽게 읽어올 수 있습니다. 또한 "args"를 통해 전달된 인자는 "immutable(불변)"하기 때문에, 값을 변경하는 것은 불가능합니다.

## 더 보기

- [Kotlin 프로그래밍 가이드](https://kotlinlang.org/docs/reference/)
- [코틀린 공식 문서](https://kotlinlang.org/docs/)