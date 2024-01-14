---
title:    "Kotlin: 컴퓨터 프로그래밍: 명령어 인자 읽기"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## 왜

커맨드 라인 인수를 읽는 것의 중요성은 간단합니다. 여기에 그 주요 이유가 있습니다: 시스템 매개변수 혹은 사용자로부터 받은 입력을 바탕으로 프로그램이 다르게 동작하도록 설정할 수 있기 때문입니다.

## 어떻게

```Kotlin
fun main(args: Array<String>) {
    if (args.isNotEmpty()) {
        println("안녕하세요, ${args[0]}님")
    } else {
        println("인사해줄 사람의 이름을 입력해주세요")
    }
}
```

위 예시 코드는 입력받은 커맨드 라인 인수의 첫 번째 요소를 활용하여 해당하는 사용자에게 인사하는 간단한 프로그램입니다. 코드 안에서 `args` 배열을 사용하여 커맨드 라인 인수를 읽을 수 있습니다.

```
$ kotlinc CommandLineArguments.kt -include-runtime -d CommandLineArguments.jar
$ java -jar CommandLineArguments.jar John
안녕하세요, John님
$ java -jar CommandLineArguments.jar Emily
안녕하세요, Emily님
```

## 깊이 파고들기

커맨드 라인 인수를 읽는 방법에 대해 좀 더 자세히 알아보겠습니다. Kotlin에서는 `main` 함수의 매개변수로 `args` 배열을 사용하는 것으로 간단하게 커맨드 라인 인수를 읽을 수 있습니다. 이 배열의 길이를 통해 입력받은 인수의 개수를 파악할 수 있으며, `args[0]`부터 순서대로 인수를 읽을 수 있습니다. 또한 `args` 배열 내의 각 인수는 `String` 타입으로 저장되어 있기 때문에 다양한 형태의 입력도 처리할 수 있습니다.

하지만 `args` 배열을 직접 다루기 보다는 `kotlin.system` 라이브러리의 `commandLineArgs` 함수를 사용하면 더 편리합니다. 이 함수는 `String` 타입의 커맨드 라인 인수들을 `List` 타입으로 반환해줍니다. 따라서 `List` 타입의 다양한 함수들을 사용하여 커맨드 라인 인수를 처리할 수 있습니다.

## 더 알아보기

Kotlin에서 커맨드 라인 인수를 다루는 더 많은 방법과 기능을 알고 싶다면 아래의 링크들을 참고해보세요.

- [Kotlin 공식 문서 - Command line arguments](https://kotlinlang.org/docs/tutorials/command-line.html)
- [Kotlin Playground - Reading and parsing command line arguments in Kotlin](https://play.kotlinlang.org/byExample/01_introduction/09_Command%20line%20arguments)
- [Baeldung - Processing Command Line Arguments in Kotlin](https://www.baeldung.com/kotlin/processing-command-line-arguments)