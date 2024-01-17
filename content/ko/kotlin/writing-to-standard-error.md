---
title:                "표준 오류 쓰기"
html_title:           "Kotlin: 표준 오류 쓰기"
simple_title:         "표준 오류 쓰기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 무엇 & 왜? 
표준 오류에 쓰는 것이 무엇인지에 대한 간단한 설명과 프로그래머들이 그렇게 하는 이유에 대해 두 가지 문장으로 설명합니다. 

표준 오류에 쓰기는 프로그램에서 예외를 처리하는 중요한 방법 중 하나입니다. 이를 통해 개발자들은 프로그램에서 발생하는 오류를 쉽게 식별하고 문제를 디버깅할 수 있습니다.

## 하는 방법:
코딩 예제와 ```Kotlin ... ``` 코드 블록 내에서 샘플 출력을 제공합니다.

#### 코딩 예제:
```Kotlin
fun main() {
    println("This is standard output")
    System.err.println("This is standard error")
}

// 콘솔 출력:
This is standard output
This is standard error
```

## 깊은 곳:
표준 오류에 쓴다는 것은 프로그래밍에서 예외 처리를 위한 중요한 기술입니다. 이전에는 프로그램에서 오류가 발생했을 때 해당 오류 메시지를 출력하는 것이 전부였지만, 표준 오류에 쓰기를 통해 개발자들은 오류 내용을 더 자세히 파악하고 디버깅할 수 있게 되었습니다. 또한 표준 오류를 사용하면 표준 출력과 구분하여 오류 메시지를 로그 파일에 기록하는 것도 가능해졌습니다.

대체로 표준 오류에 쓰기는 예외 처리 이외에도 다양한 상황에서 유용하게 사용될 수 있습니다. 예를 들어, 프로그램의 진행 상황을 추적하기 위해 로그 파일에 출력할 때도 표준 오류에 쓰기를 사용합니다.

표준 오류에 쓰는 것 외에도, 개발자들은 로깅 프레임워크를 사용하여 로그 파일에 메시지를 출력할 수 있습니다. 로깅 프레임워크를 사용할 경우에는 표준 오류와 로그 파일에 동시에 출력할 수 있습니다.

## 관련 자료:
표준 오류에 쓰기와 관련된 자세한 내용은 아래 링크를 참고하세요.

- [Kotlin official documentation on system streams](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/-system.out/)
- [Article explaining the difference between standard output and error](https://www.baeldung.com/java-printstream-println-vs-print-error)
- [Comparison of different logging frameworks in Kotlin](https://logging.apache.org/log4j/log4j-2.1/manual/api.html)