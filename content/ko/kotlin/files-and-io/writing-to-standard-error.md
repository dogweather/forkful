---
title:                "표준 에러에 쓰기"
date:                  2024-02-03T19:33:54.031461-07:00
model:                 gpt-4-0125-preview
simple_title:         "표준 에러에 쓰기"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
