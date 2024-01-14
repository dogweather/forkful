---
title:    "Kotlin: 표준 오류에 쓰기"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## 왜

표준 에러에 쓰는 것에 대해 깊게 배우고 싶은 독자들을 위해 이 포스트를 준비했습니다.

## 어떻게

다음 예제 코드를 사용하여 코틀린에서 표준 에러에 쓰는 방법을 알아보겠습니다.

```Kotlin
fun main() {
    try{
        // 예외를 발생시키는 작업
    } catch (e: Exception) {
        System.err.println("에러 발생: $e")
    }
}
```

위의 코드에서 "System.err"은 표준 에러를 가리키는 출력 스트림입니다. 예외가 발생하면 catch 블록에서 해당 에러 메시지를 출력하여 디버깅에 도움을 줍니다.

## 딥 다이브

실제로는 표준 에러에 대해 더 많은 정보를 알아야 할 필요가 있습니다. 

- 표준 에러를 사용하는 이유는 무엇일까요?
- 표준 에러와 표준 출력의 차이는 무엇일까요?
- 표준 에러를 통해 디버깅하는 방법은 무엇일까요?
- 코틀린에서 표준 에러를 다루는 방법은 다른 언어와 어떤 차이가 있을까요?

이런 깊이있는 질문에 대답하기 위해 표준 에러의 기본적인 개념부터 다루는 링크를 준비했습니다.

## 참고 링크

- [코틀린 공식 문서 - 표준 에러와 표준 출력](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-println.html)
- [표준 에러를 사용하는 이유에 대한 설명 포스트](https://stackoverflow.com/questions/2340687/why-are-symbolic-names-for-standard-error-and-standard-output-not-predefined-in)
- [코틀린에서 표준 에러를 다루는 방법 상세 가이드](https://www.baeldung.com/kotlin/standard-error)
- [다른 언어와의 차이를 비교하는 포스트](https://www.geeksforgeeks.org/difference-between-system-out-println-and-system-err-println-in-java/)

## 참고 링크

- [표준 출력에 쓰는 방법에 대한 보다 자세한 설명](https://loremipsum.de/2016/04/27/writing-to-standard-error-in-java/)
- [코틀린 공식 문서 - 파일 출력 스트림 사용하기](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file-output-stream/index.html)
- [기본적인 입출력 개념에 대한 설명 포스트](https://www.geeksforgeeks.org/io-in-java-with-qualified-name-collision/)