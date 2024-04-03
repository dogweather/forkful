---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:58.714247-07:00
description: "\uBC29\uBC95: \uC544\uB450\uC774\uB178\uB294 \uBCF5\uC7A1\uD55C \uD30C\
  \uC77C \uC2DC\uC2A4\uD15C \uC791\uC5C5\uC744 \uC0C1\uC790\uC5D0\uC11C \uBC14\uB85C\
  \ \uC9C0\uC6D0\uD558\uC9C0 \uC54A\uC2B5\uB2C8\uB2E4. \uADF8\uB7EC\uB098, \uD45C\uC900\
  \ \uC544\uB450\uC774\uB178 IDE\uC758 \uC77C\uBD80\uC778 SD \uB77C\uC774\uBE0C\uB7EC\
  \uB9AC\uB97C \uC0AC\uC6A9\uD558\uBA74, \uD30C\uC77C\uACFC \uB514\uB809\uD1A0\uB9AC\
  \ \uC791\uC5C5\uC744 \uC27D\uAC8C \uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4. \uB514\
  \uB809\uD1A0\uB9AC\uAC00 \uC874\uC7AC\uD558\uB294\uC9C0 \uD655\uC778\uD558\uB824\
  \uBA74, \uBA3C\uC800 SD \uCE74\uB4DC\uB97C \uCD08\uAE30\uD654\uD55C \uB2E4\uC74C\
  \ SD \uB77C\uC774\uBE0C\uB7EC\uB9AC\uC758\u2026"
lastmod: '2024-03-13T22:44:55.628847-06:00'
model: gpt-4-0125-preview
summary: "\uC544\uB450\uC774\uB178\uB294 \uBCF5\uC7A1\uD55C \uD30C\uC77C \uC2DC\uC2A4\
  \uD15C \uC791\uC5C5\uC744 \uC0C1\uC790\uC5D0\uC11C \uBC14\uB85C \uC9C0\uC6D0\uD558\
  \uC9C0 \uC54A\uC2B5\uB2C8\uB2E4."
title: "\uB514\uB809\uD1A0\uB9AC\uAC00 \uC874\uC7AC\uD558\uB294\uC9C0 \uD655\uC778\
  \uD558\uAE30"
weight: 20
---

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
