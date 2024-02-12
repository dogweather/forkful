---
title:                "로깅"
aliases:
- /ko/kotlin/logging/
date:                  2024-01-26T01:08:31.136270-07:00
model:                 gpt-4-1106-preview
simple_title:         "로깅"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/logging.md"
---

{{< edit_this_page >}}

## 무엇인가 & 왜인가?

로깅은 소프트웨어 애플리케이션에서 이벤트와 데이터를 파일이나 콘솔과 같은 외부 출력장치에 기록하는 작업을 기본으로 합니다. 프로그래머들은 코드를 추적하거나, 문제를 해결하고, 앱의 동작을 야생에서 주시하며, 다른 방식으로는 효과적으로 파악할 수 없는 중요한 통찰력을 제공하기 위해 로깅을 합니다.

## 방법:

코틀린에서 로깅은 단순한 경우 내장된 `println()` 함수를 사용하거나, SLF4J와 Logback 또는 Log4j과 같은 보다 정교한 라이브러리를 사용하여 고급 니즈에 대해 수행할 수 있습니다.

아래는 `println()`을 사용한 기본 예제입니다:

```Kotlin
fun main() {
    println("간단한 로그 메시지: 애플리케이션이 시작되었습니다.")
    // ... 여기에 어떤 애플리케이션 로직 ...
    try {
        // 에러 시뮬레이션
        throw Exception("시뮬레이션된 에러")
    } catch (e: Exception) {
        println("오류 로그 메시지: " + e.message)
    }
}
```

출력:
```
간단한 로그 메시지: 애플리케이션이 시작되었습니다.
오류 로그 메시지: 시뮬레이션된 에러
```

그리고 SLF4J와 설정된 Logback을 사용하는 코드 스니펫은 다음과 같습니다:

```Kotlin
import org.slf4j.LoggerFactory

private val logger = LoggerFactory.getLogger("MyAppLogger")

fun main() {
    logger.info("구조화된 로그 메시지: 앱이 실행되었습니다.")
    // ... 여기에 어떤 애플리케이션 로직 ...
    try {
        // 에러 시뮬레이션
        throw Exception("시뮬레이션된 에러")
    } catch (e: Exception) {
        logger.error("구조화된 오류 로그: ", e)
    }
}
```

적절한 Logback 설정을 가정하면, 출력은 포맷되어 로그 파일에 다음과 같이 작성될 것입니다:
```
[정보] - 2023-03-29 14:15:42 - MyAppLogger - 구조화된 로그 메시지: 앱이 실행되었습니다.
[오류] - 2023-03-29 14:15:43 - MyAppLogger - 구조화된 오류 로그: 
java.lang.Exception: 시뮬레이션된 에러
   at com.myapp.Main.main(Main.kt:10)
```

## 심층 탐구

역사적으로, 로깅은 애플리케이션과 시스템의 복잡성이 증가하면서 소프트웨어 개발과 함께 진화해왔습니다. 프로그램이 개발자 자신에 의해 종종 실행되고 디버그 됐던 초기에는 단순한 프린트 문이 충분했습니다. 그러나 시스템이 네트워크화되고 다양한 사용자들이 다른 환경에서 실행됨에 따라, 견고하고 지속적인 로깅 시스템이 필수적이 되었습니다.

코틀린이 인기를 얻기 전에, 자바 개발자들은 Log4j를 널리 사용하며 나중에는 SLF4J를 받아들였습니다. 이들은 코틀린의 자바 라이브러리와의 상호 운용성을 활용하여 유사한 실무를 코틀린에 영감을 주었습니다. SLF4J는 실제 로깅 구현을 바꿀 수 있는 추상화 계층으로 작동하며, 일반적으로 Logback 또는 Log4j2가 선호되는 선택입니다.

코틀린은 또한 `expect`/`actual` 메커니즘을 통해 JVM, JavaScript, Native 등을 포괄하는 멀티 플랫폼 로깅 솔루션을 가능하게 합니다. 이 메커니즘은 플랫폼 특정 구현을 추상화합니다.

전용 로깅 라이브러리와 대조적으로, println은 추가 설정이나 종속성을 요구하지 않기 때문에 가장 간단한 로깅 형태로 남아 있지만, 로그 레벨, 로그 로테이션, 구조화된 포맷과 같은 기능이 부족하여 보통 생산 애플리케이션에는 적합하지 않습니다.

고급 로깅 프레임워크의 다른 일반적인 기능은 다음과 같습니다:

- 로그 메시지의 긴급성을 분류하는 로그 레벨(DEBUG, INFO, WARN, ERROR 등).
- 콘솔, 파일, 데이터베이스 또는 네트워크 서비스와 같은 다양한 싱크로 출력.
- 자동 로그 회전 및 보존 정책.
- 마이크로서비스 아키텍처를 위한 분산 추적 지원.
- 로그 분석 시스템과 잘 통합되는 JSON 같은 형식을 사용한 구조화된 로깅.

이러한 도구와 기능은 복잡하고, 분산되며, 또는 규모가 큰 환경에서 특히 신뢰할 수 있고 관찰 가능한 시스템을 유지하는 데 매우 중요합니다.

## 참고

코틀린 로깅에 대한 추가 학습과 통찰을 얻으려면 다음을 확인해 보세요:

- SLF4J (자바를 위한 간단한 로깅 퍼사드) [http://www.slf4j.org/](http://www.slf4j.org/)
- Logback, Log4j의 후속작 [http://logback.qos.ch/](http://logback.qos.ch/)
- Log4j 2 [https://logging.apache.org/log4j/2.x/](https://logging.apache.org/log4j/2.x/)
- 'expect' 및 'actual' 선언에 대한 코틀린 멀티플랫폼 문서: [https://kotlinlang.org/docs/multiplatform.html](https://kotlinlang.org/docs/multiplatform.html)
- 코틀린에서 구조화된 로깅 가이드: [https://ktor.io/docs/logging.html](https://ktor.io/docs/logging.html)
