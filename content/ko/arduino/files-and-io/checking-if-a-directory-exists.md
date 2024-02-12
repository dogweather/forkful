---
title:                "디렉토리가 존재하는지 확인하기"
aliases:
- /ko/arduino/checking-if-a-directory-exists.md
date:                  2024-02-03T19:06:58.714247-07:00
model:                 gpt-4-0125-preview
simple_title:         "디렉토리가 존재하는지 확인하기"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇인가 & 왜인가?
아두이노 프로그래밍의 맥락에서, SD 카드나 유사한 저장 모듈에 디렉토리가 존재하는지 확인하는 것은 오류 없이 파일을 읽거나 쓸 수 있게 해줍니다. 이 작업은 데이터 로깅, 구성 관리 또는 구조화된 파일 저장이 필요한 모든 작업에 있어 필수적이며, 애플리케이션의 신뢰성과 유연한 성능을 보장합니다.

## 방법:
아두이노는 복잡한 파일 시스템 작업을 상자에서 바로 지원하지 않습니다. 그러나, 표준 아두이노 IDE의 일부인 SD 라이브러리를 사용하면, 파일과 디렉토리 작업을 쉽게 할 수 있습니다. 디렉토리가 존재하는지 확인하려면, 먼저 SD 카드를 초기화한 다음 SD 라이브러리의 `exists()` 메소드를 사용해야 합니다.

먼저, SD 라이브러리를 포함하고 칩 선택 핀을 선언하세요:

```cpp
#include <SPI.h>
#include <SD.h>

const int chipSelect = 4; // SD 카드 모듈을 위한 칩 선택 핀
```

`setup()` 함수에서, SD 카드를 초기화하고 디렉토리가 존재하는지 확인하세요:

```cpp
void setup() {
  Serial.begin(9600);
  
  if (!SD.begin(chipSelect)) {
    Serial.println("초기화 실패!");
    return;
  }

  // 디렉토리가 존재하는지 확인
  if (SD.exists("/myDir")) {
    Serial.println("디렉토리가 존재합니다.");
  } else {
    Serial.println("디렉토리가 존재하지 않습니다.");
  }
}
```
`loop()` 함수에서, 필요에 따라 다른 작업 코드를 추가하거나 비워 둘 수 있습니다:

```cpp
void loop() {
  // 작업 코드 추가 또는 비워 둠
}
```

코드를 실행할 때 샘플 출력은 다음과 같습니다:

```
디렉토리가 존재합니다.
```
또는

```
디렉토리가 존재하지 않습니다.
```

SD 카드가 올바르게 포맷되어 있고 `/myDir` 디렉토리 경로가 특정 요구사항과 일치하는지 확인하는 것이 중요합니다. 이 기본 확인은 아두이노로 SD 카드에서 파일과 디렉토리를 사용하여 더 복잡한 작업을 수행하는 기반이 됩니다.
