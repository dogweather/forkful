---
title:                "표준 에러에 쓰기"
date:                  2024-02-03T19:33:43.302533-07:00
model:                 gpt-4-0125-preview
simple_title:         "표준 에러에 쓰기"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?
표준 오류(stderr)에 기록하기는 콘솔이나 터미널에 에러 메시지와 진단 정보를 출력하는 과정을 말합니다. 프로그래머들은 이를 통해 에러 정보를 표준 출력(stdout)과 분리하여 디버깅과 로그 분석을 용이하게 합니다.

## 어떻게:

### Java에서 기본 stderr 출력
Java는 `System.err.print()`나 `System.err.println()`을 사용하여 stderr에 기록하는 간단한 방법을 제공합니다. 다음과 같이 사용합니다:

```java
public class StdErrExample {
    public static void main(String[] args) {
        try {
            int division = 10 / 0;
        } catch (ArithmeticException e) {
            System.err.println("Error: Cannot divide by zero.");
        }
    }
}
```

출력 예:

```
Error: Cannot divide by zero.
```

이것은 오류 메시지를 직접 표준 오류 스트림에 출력합니다.

### 고급 에러 처리를 위한 로거 사용
더 정교한 에러 처리와 로깅이 필요한 애플리케이션의 경우, SLF4J와 Logback 또는 Log4J2 같은 로깅 라이브러리를 사용하는 것이 일반적입니다. 이를 통해 파일 리디렉션, 필터링, 포매팅을 포함한 에러 출력 관리의 유연성이 향상됩니다.

#### Logback 예시

먼저, `pom.xml`(Maven)이나 `build.gradle`(Gradle) 파일에 Logback 의존성을 추가합니다. Maven의 경우:

```xml
<dependency>
    <groupId>ch.qos.logback</groupId>
    <artifactId>logback-classic</artifactId>
    <version>1.2.3</version>
</dependency>
```

그런 다음, 다음 코드를 사용하여 에러를 로깅할 수 있습니다:

```java
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class LoggerExample {
    private static final Logger logger = LoggerFactory.getLogger(LoggerExample.class);
    
    public static void main(String[] args) {
        try {
            int result = 10 / 0;
        } catch (ArithmeticException e) {
            logger.error("Error: Cannot divide by zero.", e);
        }
    }
}
```

이것은 Logback 구성에 따라 콘솔이나 파일에 오류 메시지와 스택 트레이스를 출력합니다.

Logback 같은 로깅 프레임워크를 사용하면 에러 처리를 더욱 효율적으로 제어할 수 있어 대규모 애플리케이션과 시스템을 관리하기가 더 쉬워집니다.
