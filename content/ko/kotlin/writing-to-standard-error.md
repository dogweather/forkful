---
title:                "Kotlin: 표준 오류에 작성하기"
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 왜
스탠다드 에러에 대한 글을 쓰는 이유는 무엇일까요? 에러 메시지는 프로그래밍의 필수 요소이지만, 올바른 처리가 이루어지지 않으면 불필요한 시간과 노력을 낭비하게 됩니다. 따라서 스탠다드 에러에 대한 이해는 프로그래밍의 중요한 부분이 됩니다.

## 진행 방법
프로그래밍 언어 Kotlin에서 스탠다드 에러를 작성하는 방법을 알아보겠습니다. 먼저 `System.err`를 사용하여 error 객체를 생성하고, `println()` 메서드를 통해 에러 메시지를 출력합니다. 아래는 간단한 예제 코드와 출력 결과입니다.

```Kotlin
val error = System.err
error.println("에러가 발생했습니다.")
```

```
에러가 발생했습니다.
```

## 깊게 들어가기
실제로 프로그래밍을 하면서 스탠다드 에러를 만나게 될 때, 에러 메시지가 어떤 내용을 담고 있는지 이해하는 것이 중요합니다. 에러 메시지는 프로그램의 동작에 대한 정보를 제공해주기 때문에, 적절한 대처를 할 수 있도록 반드시 확인해야 합니다. 또한 에러 메시지의 종류에 따라 처리 방법이 달라질 수 있으므로, 자세한 정보를 얻기 위해선 문서를 참고하는 것이 좋습니다.

## 같이 보기
다른 유용한 정보를 얻기 위해 아래 링크를 확인해보세요.

* Kotlin 공식 문서 - [https://kotlinlang.org/docs/](https://kotlinlang.org/docs/)
* 스탠다드 에러 처리 방법 - [https://www.javatpoint.com/exception-handling-in-kotlin](https://www.javatpoint.com/exception-handling-in-kotlin)
* Kotlin 기초 문법 - [https://www.tutorialspoint.com/kotlin/index.htm](https://www.tutorialspoint.com/kotlin/index.htm)

## 같이 보기