---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:54.031461-07:00
description: "\uD45C\uC900 \uC624\uB958(stderr)\uC5D0 \uC791\uC131\uD55C\uB2E4\uB294\
  \ \uAC83\uC740 \uC5D0\uB7EC \uBA54\uC2DC\uC9C0\uC640 \uC9C4\uB2E8\uC744 \uD45C\uC900\
  \ \uCD9C\uB825(stdout)\uACFC \uBCC4\uB3C4\uC758 \uBCC4\uB3C4 \uC2A4\uD2B8\uB9BC\uC73C\
  \uB85C \uCD9C\uB825\uD558\uC5EC \uB354 \uB098\uC740 \uC5D0\uB7EC \uCC98\uB9AC\uC640\
  \ \uB85C\uADF8 \uD30C\uC2F1\uC744 \uAC00\uB2A5\uD558\uAC8C \uD558\uB294 \uAC83\uC785\
  \uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uB514\uBC84\uAE45\uC744\
  \ \uC6A9\uC774\uD558\uAC8C \uD558\uACE0, \uC624\uB958 \uBA54\uC2DC\uC9C0\uB97C \uC27D\
  \uAC8C \uC2DD\uBCC4 \uBC0F \uD544\uC694\uD55C \uACBD\uC6B0 \uB9AC\uB514\uB809\uC158\
  \ \uD560\u2026"
lastmod: '2024-03-11T00:14:29.114348-06:00'
model: gpt-4-0125-preview
summary: "\uD45C\uC900 \uC624\uB958(stderr)\uC5D0 \uC791\uC131\uD55C\uB2E4\uB294 \uAC83\
  \uC740 \uC5D0\uB7EC \uBA54\uC2DC\uC9C0\uC640 \uC9C4\uB2E8\uC744 \uD45C\uC900 \uCD9C\
  \uB825(stdout)\uACFC \uBCC4\uB3C4\uC758 \uBCC4\uB3C4 \uC2A4\uD2B8\uB9BC\uC73C\uB85C\
  \ \uCD9C\uB825\uD558\uC5EC \uB354 \uB098\uC740 \uC5D0\uB7EC \uCC98\uB9AC\uC640 \uB85C\
  \uADF8 \uD30C\uC2F1\uC744 \uAC00\uB2A5\uD558\uAC8C \uD558\uB294 \uAC83\uC785\uB2C8\
  \uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uB514\uBC84\uAE45\uC744 \uC6A9\
  \uC774\uD558\uAC8C \uD558\uACE0, \uC624\uB958 \uBA54\uC2DC\uC9C0\uB97C \uC27D\uAC8C\
  \ \uC2DD\uBCC4 \uBC0F \uD544\uC694\uD55C \uACBD\uC6B0 \uB9AC\uB514\uB809\uC158 \uD560\
  \u2026"
title: "\uD45C\uC900 \uC5D0\uB7EC\uC5D0 \uC4F0\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?

표준 오류(stderr)에 작성한다는 것은 에러 메시지와 진단을 표준 출력(stdout)과 별도의 별도 스트림으로 출력하여 더 나은 에러 처리와 로그 파싱을 가능하게 하는 것입니다. 프로그래머들은 디버깅을 용이하게 하고, 오류 메시지를 쉽게 식별 및 필요한 경우 리디렉션 할 수 있도록 하여, 깨끗한 출력 로그나 사용자 메시지를 유지하기 위해 이렇게 합니다.

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
