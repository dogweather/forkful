---
date: 2024-01-26 01:07:23.137539-07:00
description: "\uBC29\uBC95: \uB2E4\uC74C\uC740 \uB0B4\uC7A5\uB41C `java.util.logging`\
  \ \uD328\uD0A4\uC9C0\uB97C \uC0AC\uC6A9\uD558\uC5EC \uC790\uBC14\uC5D0\uC11C \uB85C\
  \uAE45\uC744 \uC2DC\uC791\uD558\uB294 \uAC04\uB2E8\uD55C \uBC29\uBC95\uC785\uB2C8\
  \uB2E4."
lastmod: '2024-03-13T22:44:55.058325-06:00'
model: gpt-4-1106-preview
summary: "\uB2E4\uC74C\uC740 \uB0B4\uC7A5\uB41C `java.util.logging` \uD328\uD0A4\uC9C0\
  \uB97C \uC0AC\uC6A9\uD558\uC5EC \uC790\uBC14\uC5D0\uC11C \uB85C\uAE45\uC744 \uC2DC\
  \uC791\uD558\uB294 \uAC04\uB2E8\uD55C \uBC29\uBC95\uC785\uB2C8\uB2E4."
title: "\uB85C\uAE45"
weight: 17
---

## 방법:
다음은 내장된 `java.util.logging` 패키지를 사용하여 자바에서 로깅을 시작하는 간단한 방법입니다.

```java
import java.util.logging.Logger;
import java.util.logging.Level;

public class AppLogging {
    private final static Logger LOGGER = Logger.getLogger(Logger.GLOBAL_LOGGER_NAME);

    public static void main(String[] args) {
        LOGGER.info("INFO 수준의 메시지 로깅");

        try {
            int division = 10 / 0;
        } catch (ArithmeticException e) {
            LOGGER.log(Level.SEVERE, "예외 발생", e);
        }
    }
}
```

이 코드는 다음과 같은 출력을 생성합니다:

```
Jul 03, 2023 2:00:00 PM AppLogging main
INFO: INFO 수준의 메시지 로깅
Jul 03, 2023 2:00:00 PM AppLogging main
SEVERE: 예외 발생
java.lang.ArithmeticException: / by zero
    at AppLogging.main(AppLogging.java:10)
```

## 깊이 있게 파보기
자바의 로깅은 상당히 발전해 왔습니다. 역사적으로, 로깅은 시스템 출력이나 자체 작성된 메커니즘과 같은 자의적인 방법으로 이루어졌습니다. 하지만, 표준화의 필요성은 `Log4j` 및 `SLF4J`와 같은 로깅 API로 이끌었습니다. `java.util.logging` 패키지는 JDK 1.4에 도입되어 메시지를 로그하는 표준 방법을 제공했습니다.

`java.util.logging` (JUL) 대안으로는 Log4j 2와 SLF4J가 있습니다. JUL은 자바에 내장되어 있어 추가 의존성이 필요 없지만, Log4j 2와 SLF4J는 로깅 설정에 대한 보다 세부적인 제어, 비동기 로깅, 그리고 더 나은 성능 등 보다 고급 기능을 제공합니다.

구현 측면에서 로깅은 동기식, 즉 생성된 스레드에서 각 로그 메시지가 처리되거나, 비동기식, 즉 메시지가 별도의 스레드로 넘겨지게 됩니다. 비동기 로깅은 성능을 향상시킬 수 있지만, 동시성을 처리하고 애플리케이션이 충돌할 때 로그 메시지가 유실되지 않도록 해야 하므로 복잡성을 소개합니다.

## 참고 자료
- [Log4j 2](https://logging.apache.org/log4j/2.x/)
- [SLF4J](http://www.slf4j.org/)
- [오라클의 공식 로깅 개요](https://docs.oracle.com/javase/8/docs/technotes/guides/logging/overview.html)
- [java.util.logging에 대한 튜토리얼](https://www.vogella.com/tutorials/Logging/article.html)
