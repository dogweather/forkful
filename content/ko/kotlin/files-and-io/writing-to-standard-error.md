---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:54.031461-07:00
description: "\uC5B4\uB5BB\uAC8C: Kotlin\uC5D0\uC11C\uB294 `System.err.println()`\uC744\
  \ \uC0AC\uC6A9\uD558\uC5EC stderr\uC5D0 \uC791\uC131\uD560 \uC218 \uC788\uC2B5\uB2C8\
  \uB2E4. \uC774 \uBA54\uC11C\uB4DC\uB294 `System.out.println()`\uACFC \uC720\uC0AC\
  \uD558\uC9C0\uB9CC \uCD9C\uB825\uC744 \uD45C\uC900 \uCD9C\uB825 \uC2A4\uD2B8\uB9BC\
  \uC774 \uC544\uB2CC \uD45C\uC900 \uC624\uB958 \uC2A4\uD2B8\uB9BC\uC73C\uB85C \uBCF4\
  \uB0C5\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.195532-06:00'
model: gpt-4-0125-preview
summary: "Kotlin\uC5D0\uC11C\uB294 `System.err.println()`\uC744 \uC0AC\uC6A9\uD558\
  \uC5EC stderr\uC5D0 \uC791\uC131\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "\uD45C\uC900 \uC5D0\uB7EC\uC5D0 \uC4F0\uAE30"
weight: 25
---

## 어떻게:
Kotlin에서는 `System.err.println()`을 사용하여 stderr에 작성할 수 있습니다. 이 메서드는 `System.out.println()`과 유사하지만 출력을 표준 출력 스트림이 아닌 표준 오류 스트림으로 보냅니다.

```kotlin
fun main() {
    System.err.println("This is an error message!")
}
```

샘플 출력:
```
This is an error message!
```

Logback이나 SLF4J 같은 로깅 프레임워크를 포함하는 더 구조화되거나 복잡한 애플리케이션의 경우, 일정 로그 레벨(예: ERROR)에 대해 오류를 stderr에 작성하도록 로거를 구성할 수 있습니다.

SLF4J와 Logback 사용하기:

1. 먼저, SLF4J API와 Logback 구현을 `build.gradle`에 추가합니다:

```groovy
dependencies {
    implementation 'org.slf4j:slf4j-api:1.7.30'
    implementation 'ch.qos.logback:logback-classic:1.2.3'
}
```

2. 다음으로, 오류 레벨 메시지를 stderr로 보내도록 Logback을(`src/main/resources/logback.xml`에 있음) 구성합니다:

```xml
<configuration>
    <appender name="STDERR" class="ch.qos.logback.core.ConsoleAppender">
        <target>System.err</target>
        <encoder>
            <pattern>%d{yyyy-MM-dd HH:mm:ss} [%thread] %-5level %logger{36} - %msg%n</pattern>
        </encoder>
    </appender>
    
    <root level="error">
        <appender-ref ref="STDERR" />
    </root>
</configuration>
```

3. 그런 다음, Kotlin 코드에서 SLF4J를 사용하여 오류 메시지를 로깅합니다:

```kotlin
import org.slf4j.LoggerFactory

fun main() {
    val logger = LoggerFactory.getLogger("ExampleLogger")
    logger.error("This is an error log message!")
}
```

stderr로의 샘플 출력:
```
2023-04-01 12:34:56 [main] ERROR ExampleLogger - This is an error log message!
```
