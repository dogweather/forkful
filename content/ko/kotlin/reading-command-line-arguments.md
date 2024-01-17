---
title:                "컴퓨터 프로그래밍에서 명령 줄 인자 읽기"
html_title:           "Kotlin: 컴퓨터 프로그래밍에서 명령 줄 인자 읽기"
simple_title:         "컴퓨터 프로그래밍에서 명령 줄 인자 읽기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# 커맨드 라인 인수 읽기란 무엇이며 왜 필요한가요?

커맨드 라인 인수를 읽는 것은 프로그램이 시작될 때 사용자가 입력한 인수를 읽어들이는 작업을 의미합니다. 보통 사용자가 프로그램을 실행할 때 명령행에 추가적인 정보를 넣어주기 위해 사용됩니다. 이를 통해 사용자가 원하는 결과를 얻을 수 있도록 프로그램을 조작할 수 있습니다.

## 어떻게 하나요?

**코틀린**에서는 ```args``` 배열을 통해 커맨드 라인 인수를 읽을 수 있습니다. 다음은 인수를 출력하는 예시 코드입니다.

```kotlin
fun main(args: Array<String>) {
    println("입력한 인수는 다음과 같습니다:")
    for (arg in args) {
        println(arg)
    }
}
```

사용자가 프로그램을 실행할 때 다음과 같이 인수를 추가할 수 있습니다:

```
kotlin MyProgram.kt arg1 arg2
```

위 코드를 실행하면 다음과 같은 결과가 출력됩니다:

```
입력한 인수는 다음과 같습니다:
arg1
arg2
```

## 깊이있게 살펴보기

### 역사적 배경

커맨드 라인 인수를 읽는 기능은 오래 전부터 존재했습니다. 과거에는 프로그램을 실행할 때 모든 인수를 미리 설정해야 했기 때문에, 유연성과 편의성 측면에서 커맨드 라인 인수가 중요한 역할을 했습니다.

### 대안

커맨드 라인 인수를 읽는 대안으로는 프롬프트를 이용하여 사용자로부터 직접 인수를 입력 받는 방법이 있습니다. 하지만 효율성과 사용자의 불편함 측면에서 커맨드 라인 인수를 사용하는 것이 더 좋은 방법입니다.

### 구현 세부 사항

코틀린에서는 ```args``` 배열을 통해 커맨드 라인 인수를 읽을 수 있지만, 다른 언어에서는 다른 방식으로 구현될 수도 있습니다. 프로그래밍 언어마다 다른 방식이 존재하며 개발자의 취향에 따라 다른 방식을 선택할 수 있습니다.

## 관련 자료들

- [Kotlin 공식 문서 - 커맨드 라인 인수](https://kotlinlang.org/docs/reference/parameters.html#command-line-arguments)
- [코틀린으로 Java 프로그램 작성하기](https://mvpjava.com/kotlin-tutorial-basics/) - 커맨드 라인 인수 읽는 방법 알아보기
- [Java와 Kotlin의 차이점](https://javaexpert.tistory.com/827) - 각 언어에서 커맨드 라인 인수 읽는 방법 비교하기