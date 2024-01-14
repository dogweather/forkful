---
title:    "Kotlin: 디버그 출력 출력"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

## 왜

디버그 출력을 프린팅하는 것이 왜 유용한지 궁금하셨나요? 디버깅은 개발자에게 있어서 중요한 과정입니다. 코드에서 발생하는 문제를 해결하기 위해서는 어떤 부분에서 문제가 발생하는지 알아야 합니다. 디버그 출력을 프린팅함으로써 이를 파악할 수 있습니다. 또한, 코드를 수정하고 재실행할 필요 없이 실행 중인 코드에서 문제를 파악할 수 있도록 도와줍니다.

## 방법

 디버그 출력을 프린팅하는 방법은 매우 간단합니다. 우선 코드에서 출력하고 싶은 변수나 메시지를 ```println()``` 함수를 사용하여 출력합니다. 예를 들어, 다음 코드와 같이 작성할 수 있습니다.
 
 ```Kotlin
 val name = "Jane"
 println("Name: $name")
 ```
 
 위의 코드를 실행하면 콘솔에서 "Name: Jane"이라는 메시지를 볼 수 있습니다. 또한, 변수의 값을 확인하거나 현재 실행되고 있는 코드의 어떤 부분에서 문제가 발생하는지 알 수 있도록 원하는 위치에 디버그 출력을 추가할 수 있습니다.

 ## 딥 다이브

 디버그 출력은 개발 과정에서 꼭 필요한 도구입니다. 그렇기 때문에 Kotlin에서는 여러 가지 방법으로 디버그 출력을 지원합니다. 예를 들어, ```Log``` 클래스를 사용하면 디버그 출력 메시지를 일종의 로그로 기록함으로써 나중에 참고할 수 있습니다. 또한, ```println()``` 함수 대신에 ```log()``` 함수를 사용하면 더 자세한 내용의 로그를 출력할 수 있습니다. 그리고 Kotlin에서는 문자열 보간이라는 편리한 기능을 제공하는데, 이를 활용하면 변수의 값을 문자열 안에 쉽게 삽입할 수 있습니다.

## See Also

디버그 출력에 대해 더 알고 싶나요? 아래의 링크를 확인해보세요!

- [Kotlin 공식 문서 - Logging](https://kotlinlang.org/docs/reference/logging.html)
- [Baeldung - Debugging in Kotlin](https://www.baeldung.com/kotlin/debugging)