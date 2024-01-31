---
title:                "로깅"
date:                  2024-01-26T01:00:48.066042-07:00
model:                 gpt-4-1106-preview
simple_title:         "로깅"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/logging.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
로깅은 본질적으로 프로그램이 무엇을 하는지 기록하는 것으로, 일반적으로 파일이나 터미널에 메시지를 출력함으로써 수행됩니다. 프로그래머들은 이벤트를 추적하고, 문제를 진단하며, 시간이 지남에 따른 어플리케이션의 작동 이력을 알려주는 감사 트레일(audit trail)을 가지기 위해 이를 수행합니다.

## 방법:
기본부터 시작해봅시다. C에는 내장된 로깅 프레임워크가 없지만, `stdio.h`를 사용해서 간단한 것을 만들 수 있습니다. 다음과 같이 하면 됩니다:

```c
#include <stdio.h>
#include <time.h>

void logMessage(const char* message) {
    time_t now;
    time(&now);
    char *date = ctime(&now);
    date[strlen(date) - 1] = '\0'; // ctime() 결과의 끝에 있는 개행 문자 제거
    printf("[%s] %s\n", date, message);
}

int main() {
    logMessage("Application has started.");
    // ... 여기에 코드를 작성 ...
    logMessage("Application is doing something important.");
    // ... 코드를 계속 작성 ...
    logMessage("Application has ended.");
    return 0;
}
```

출력 예시는 다음과 같을 수 있습니다:

```
[Tue Mar 9 12:00:01 2023] Application has started.
[Tue Mar 9 12:00:02 2023] Application is doing something important.
[Tue Mar 9 12:00:03 2023] Application has ended.
```

물론 실제 환경에서는 터미널 대신 파일에 쓰거나, 다른 로그 레벨을 처리하거나, 미리 정의된 라이브러리를 사용할 것입니다.

## 심층 분석
C에서의 로깅은 그 매력이 고풍스럽습니다—언어의 대부분을 구성하는 저수준(low-level) 작업과 같습니다. 역사적으로, 로깅은 `stderr` 또는 파일 포인터와 함께 `fprintf`를 사용하여 수행되었습니다. 프로그램이 복잡해짐에 따라 로깅 요구 사항도 복잡해졌고, 여러 출처의 로깅을 다루고 중요도 수준을 다루는 `syslog`와 같은 유닉스 시스템의 라이브러리 개발로 이어졌습니다.

현대 환경에서는 로그 회전(log rotation), 구조화된 로깅, 멀티스레드 로깅을 포함한 풍부한 기능 세트를 제공하는 다양한 C 로깅 라이브러리들이 많이 있습니다. 예를 들어 `zlog`, `log4c`, `glog`와 같은 것들은 로그의 장황함, 목적지, 형식을 잘 관리할 수 있게 합니다.

로깅 시스템을 구현할 때는 타임스탬프 형식화, 로그 파일 관리, 성능과 같은 세부 사항을 고려해야 합니다. 로그에 타임스탬프를 기록하는 것은 이벤트 상관 관계를 파악하는데 필수적이며, 로그 회전은 로그 파일이 너무 많은 디스크 공간을 사용하지 않도록 합니다. 로깅 작업은 또한 메인 어플리케이션의 흐름을 방해하지 않는 빠르고, 논블로킹(non-blocking) 해야 로깅이 병목 현상이 되지 않게 합니다.

## 더 보기
C의 로깅 라이브러리와 관행에 대해 더 깊이 파고들고 싶다면, 다음 자료들을 확인하세요:

- GNU `syslog` 매뉴얼: https://www.gnu.org/software/libc/manual/html_node/Syslog.html
- `zlog`: C를 위한 매우 설정 가능한 로깅 라이브러리 - https://github.com/HardySimpson/zlog
- `log4c`: Log4j를 모델로 한 C용 로깅 프레임워크 - http://log4c.sourceforge.net/
- `glog`: 구글의 어플리케이션 레벨 로깅 라이브러리 - https://github.com/google/glog
