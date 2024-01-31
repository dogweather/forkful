---
title:                "표준 오류로 쓰기"
date:                  2024-01-19
html_title:           "Bash: 표준 오류로 쓰기"
simple_title:         "표준 오류로 쓰기"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? (무엇인가 & 왜 사용하는가?)
표준 에러로 쓰기는 프로그램의 에러 메시지를 출력하는 것이다. 프로그래머들은 디버깅을 용이하게 하고, 출력과 에러를 분리하기 위해 이를 사용한다.

## How to: (어떻게 사용하는가?)
```kotlin
fun main() {
    println("정상적인 출력 메시지")
    System.err.println("에러 메시지")
}
```
출력 예시:
```
정상적인 출력 메시지
에러 메시지
```

## Deep Dive (심층 분석)
표준 에러(stdout)와 대조적으로 표준 에러(stderr)는 오류와 로그 작업을 위해 예약되어 있다. 옛날에는 이 두 스트림이 다른 장치(예를 들어, 프린터와 모니터)로 출력될 수 있었다. `System.err`는 `PrintStream` 객체이며, 자바의 `System` 클래스에서 상속된 것이다. 대안으로는 로깅 프레임워크를 사용할 수 있으나, 간단한 스크립트나 테스트 시에는 `System.err` 사용이 편리하다.

## See Also (참고자료)
- [Kotlin 공식 문서](https://kotlinlang.org/docs/home.html)
- [표준 출력과 표준 에러에 대한 자세한 정보](https://en.wikipedia.org/wiki/Standard_streams)
- [자바 PrintStream 클래스](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/io/PrintStream.html)
