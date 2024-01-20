---
title:                "명령줄 인수 읽기"
html_title:           "Arduino: 명령줄 인수 읽기"
simple_title:         "명령줄 인수 읽기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 그런가? 

명령 줄 인수를 읽는 것은 사용자가 프로그램을 시작할 때 추가로 제공하는 입력 값을 프로그램에서 사용할 수 있게 처리하는 과정입니다. 이는 특정 작업에 대한 세부 정보를 사용자에게부터 제공받거나, 프로그램 작동 방식을 쉽게 수정하려는 프로그래머들에게 굉장히 유용합니다.

## 어떻게 해야 할까?

다음은 코틀린 프로그램에서 명령 줄 인수를 읽는 방법에 대한 간단한 예입니다.

```Kotlin
fun main(args: Array<String>) {
    args.forEach { arg ->
        println(arg)
    }
}
```

이 코드를 실행하면서 명령 줄 인수를 추가하면, 그 인수들이 모두 출력됩니다. 예를 들어, 프로그램을 `kotlin MainKt arg1 arg2 arg3`와 같은 방식으로 시작하면, 출력은 다음과 같이 될 것입니다:

```
arg1
arg2
arg3
```

## 깊이있게 살펴보기 

명령 줄 인수는 운영 체제가 프로그램을 시작할 때 해당 프로그램으로 전달하는 설정 정보입니다. 이 전통은 유닉스의 초기 버전에서 시작되었으며, 현재도 널리 사용되고 있습니다.

대안으로는 환경 변수나 구성 파일을 사용하는 것이 있습니다. 하지만, 명령 줄 인수는 프로그램을 시작할 때 한 번에 그 설정을 전달하는 데에 가장 적합합니다.

코틀린에서 명령 줄 인수를 읽는 방법은 Java와 매우 비슷합니다. 코틀린의 `main` 함수의 첫 번째 매개변수는 `args: Array<String>`인데, 이는 프로그램에 전달된 모든 명령 줄 인수를 담고 있는 문자열 배열입니다.

## 참고 자료 

- 코틀린 공식 문서 (https://kotlinlang.org/docs/command-line.html)
- 코틀린에서의 명령 줄 인수 사용 (https://www.baeldung.com/kotlin-command-line-arguments)