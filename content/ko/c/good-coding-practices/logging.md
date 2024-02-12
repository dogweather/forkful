---
title:                "로깅"
aliases:
- /ko/c/logging.md
date:                  2024-02-03T17:59:13.955970-07:00
model:                 gpt-4-0125-preview
simple_title:         "로깅"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/logging.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?

C에서 로깅은 프로그램이 실행되는 동안의 흐름과 주목할 만한 이벤트를 기록하는 것을 포함하며, 이를 통해 프로그램의 행동과 성능을 실제로 검토할 수 있습니다. 프로그래머들은 디버깅 목적, 소프트웨어 건강 모니터링, 그리고 시스템 보안을 보장하기 위해 로깅을 활용합니다.

## 방법:

C에서 로깅은 기본 파일 작업이나 좀 더 정교한 라이브러리를 사용하여 달성될 수 있습니다. 간단함을 위해, 우리는 표준 입출력 라이브러리부터 시작할 것입니다. 다음의 코드 조각들은 기본 로깅 구현을 보여줍니다.

간단한 메시지를 로깅하기 위해:

```c
#include <stdio.h>

int main() {
    FILE *logFile;
    logFile = fopen("application.log", "a"); // 로그 파일을 추가 모드로 열기
    
    if (logFile == NULL) {
        perror("로그 파일을 여는 데 에러 발생.");
        return -1;
    }
    
    fprintf(logFile, "응용 프로그램 시작.\n");
    
    // 여러분의 애플리케이션 로직이 여기에
    
    fprintf(logFile, "응용 프로그램이 성공적으로 종료됨.\n");
    fclose(logFile);
    
    return 0;
}
```

`application.log`에 있는 출력:

```
응용 프로그램 시작.
응용 프로그램이 성공적으로 종료됨.
```

타임스탬프와 로그 레벨을 포함한 보다 상세한 로그를 포함하기 위해:

```c
#include <stdio.h>
#include <time.h>

void logMessage(FILE *logFile, const char* level, const char* message) {
    time_t now;
    time(&now);
    char* datetime = ctime(&now);
    datetime[strlen(datetime)-1] = '\0'; // 개행 문자 제거
    fprintf(logFile, "[%s] %s - %s\n", datetime, level, message);
}

int main() {
    FILE *logFile;
    logFile = fopen("detailed.log", "a");
    
    if (logFile == NULL) {
        perror("로그 파일을 여는 데 에러 발생.");
        return -1;
    }
    
    logMessage(logFile, "INFO", "응용 프로그램 시작");
    // 여러분의 애플리케이션 로직이 여기에
    logMessage(logFile, "ERROR", "예제 에러");
    
    fclose(logFile);
    
    return 0;
}
```

`detailed.log`에 있는 출력:

```
[Thu Mar 10 14:32:01 2023] INFO - 응용 프로그램 시작
[Thu Mar 10 14:32:02 2023] ERROR - 예제 에러
```

## 심층 분석

보여진 바와 같이, C에서의 로깅은 단순 파일 작업에 의존하며, 이는 효과적이지만 Python의 `logging` 모듈이나 Java의 `Log4j`와 같은 다른 언어에서의 로깅 기능보다 강력하거나 유연하지는 않습니다. C에서 더 진보된 로깅 능력을 위해, 개발자들은 종종 Unix 계열 시스템에서 시스템 전체 로그 관리를 제공하는 `syslog`나 `log4c`와 같은 서드 파티 라이브러리로 전환합니다.

역사적으로, 로깅은 프로그래밍의 중요한 부분이었으며, 프로그램 흐름과 오류를 추적하고 이해하는 것은 주로 물리적인 출력물을 통해 이루어졌습니다. 시스템이 발전함에 따라, 로깅은 이제 심각성의 다양한 수준, 로그 회전, 그리고 비동기 로깅을 지원하는 등 더 정교해졌습니다.

C의 표준 라이브러리는 로깅을 구현하기 위한 기본 도구를 제공하지만, 그 한계는 종종 사용자 정의 로깅 프레임워크의 생성이나 보다 풍부한 기능과 유연한 로깅 솔루션을 위한 외부 라이브러리의 채택으로 이어집니다. 이러한 한계에도 불구하고, 특히 외부 의존성을 최소화해야 하는 환경에서는 디버깅과 소프트웨어 유지관리를 위해 C에서 기본 로깅을 이해하고 구현하는 것이 중요합니다.
