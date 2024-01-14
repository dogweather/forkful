---
title:                "Kotlin: '컴퓨터 프로그래밍에서 명령줄 인수 읽기'"
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# 왜: 
코틀린에서 커맨드 라인 인자를 읽는 것은 많은 유용한 기능 중 하나입니다. 이 기능을 배우면 다양한 용도로 활용할 수 있으며, 프로그래밍 작업을 더욱 빠르고 효율적으로 수행할 수 있습니다.

## 사용 방법: 
코틀린에서 커맨드 라인 인자를 읽는 방법은 간단합니다. ```main()``` 함수의 매개변수로 ```args: Array<String>```를 추가하기만 하면 됩니다. 아래는 이 방법의 예시입니다.

```Kotlin
fun main(args: Array<String>) {
    println("입력한 인자는 ${args[0]}입니다.")
}
```

이 코드를 실행할 경우, 커맨드 라인에서 입력한 첫 번째 인자가 출력됩니다. 예를 들어, ```kotlin Main.kt hello```를 입력하면 ```입력한 인자는 hello입니다.```라는 결과가 나오게 됩니다.

또한, 여러 개의 인자를 입력한 경우 for문을 활용해 모든 인자를 출력할 수도 있습니다.

```Kotlin
fun main(args: Array<String>) {
    for (arg in args) {
        println(arg)
    }
}
```

이 코드를 실행하면 모든 입력된 인자가 한 줄씩 출력됩니다.

## 심화 학습: 
코틀린에서 커맨드 라인 인자를 읽는 방법은 다양한 방법이 있습니다. 예를 들어, ```args``` 배열의 ```size``` 프로퍼티를 활용하면 입력된 인자의 개수를 알 수 있습니다. 또한, ```args``` 배열의 ```contains()``` 메소드를 활용하면 특정 인자가 입력되었는지 확인할 수도 있습니다.

더 깊이 들어가 보면, 코틀린에서는 ```CommandLine.kt```라는 라이브러리를 통해 커맨드 라인 인자를 더욱 효율적으로 처리할 수 있습니다. 이 라이브러리를 사용하면 인자들을 구분하고 필터링할 수 있으며, 인자들을 자동으로 타입변환해주는 기능도 제공합니다.

# 더 알아보기: 
코틀린에서 커맨드 라인 인자를 읽는 방법에 대해 더 자세히 알아보려면 아래의 링크들을 참고해보세요.

- [코틀린 공식 문서](https://kotlinlang.org/docs/command-line.html)
- [많은 예시와 함께 설명되어있는 엔트리](https://medium.com/@nataschabruno/kotlin-tutorial-command-line-arguments-e51eef6a5de3)
- [```.option()``` 메소드를 사용하여 설명하는 블로그 글](https://juejin.cn/post/6844904021605325832)