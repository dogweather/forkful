---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:07.238092-07:00
description: "\uBC29\uBC95: \uC544\uB450\uC774\uB178\uB97C \uC0AC\uC6A9\uD558\uC5EC\
  \ SD \uCE74\uB4DC\uC5D0 \uD14D\uC2A4\uD2B8 \uD30C\uC77C\uC744 \uC4F0\uB824\uBA74\
  , \uBA3C\uC800 SD \uCE74\uB4DC\uC640 \uC0C1\uD638\uC791\uC6A9\uD558\uB294 \uB370\
  \ \uD544\uC694\uD55C \uD568\uC218\uB97C \uC81C\uACF5\uD558\uB294 `SD.h` \uB77C\uC774\
  \uBE0C\uB7EC\uB9AC\uB97C \uD3EC\uD568\uD574\uC57C \uD569\uB2C8\uB2E4. \uC544\uB450\
  \uC774\uB178 \uBCF4\uB4DC\uAC00 SD \uCE74\uB4DC \uBAA8\uB4C8\uC5D0 \uC5F0\uACB0\uB418\
  \uC5B4 \uC788\uB294\uC9C0 \uD655\uC778\uD558\uC138\uC694."
lastmod: '2024-03-13T22:44:55.635058-06:00'
model: gpt-4-0125-preview
summary: "\uC544\uB450\uC774\uB178\uB97C \uC0AC\uC6A9\uD558\uC5EC SD \uCE74\uB4DC\uC5D0\
  \ \uD14D\uC2A4\uD2B8 \uD30C\uC77C\uC744 \uC4F0\uB824\uBA74, \uBA3C\uC800 SD \uCE74\
  \uB4DC\uC640 \uC0C1\uD638\uC791\uC6A9\uD558\uB294 \uB370 \uD544\uC694\uD55C \uD568\
  \uC218\uB97C \uC81C\uACF5\uD558\uB294 `SD.h` \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C\
  \ \uD3EC\uD568\uD574\uC57C \uD569\uB2C8\uB2E4."
title: "\uD14D\uC2A4\uD2B8 \uD30C\uC77C \uC4F0\uAE30"
weight: 24
---

## 방법:
아두이노를 사용하여 SD 카드에 텍스트 파일을 쓰려면, 먼저 SD 카드와 상호작용하는 데 필요한 함수를 제공하는 `SD.h` 라이브러리를 포함해야 합니다. 아두이노 보드가 SD 카드 모듈에 연결되어 있는지 확인하세요.

```cpp
#include <SPI.h>
#include <SD.h>

File myFile;

void setup() {
  // 9600 비트 당 초 속도로 직렬 통신을 초기화합니다:
  Serial.begin(9600);
  
  // SD 카드 초기화 확인
  if (!SD.begin(4)) {
    Serial.println("Initialization failed!");
    return;
  }
  Serial.println("Initialization done.");
  
  // 파일을 엽니다. 한 번에 하나의 파일만 열 수 있음을 참고하세요,
  // 따라서 이 파일을 닫기 전에는 다른 파일을 열 수 없습니다.
  myFile = SD.open("test.txt", FILE_WRITE);
  
  // 파일을 성공적으로 열었다면, 작성합니다:
  if (myFile) {
    Serial.print("Writing to test.txt...");
    myFile.println("Testing text file write.");
    // 파일을 닫습니다:
    myFile.close();
    Serial.println("done.");
  } else {
    // 파일이 열리지 않았다면, 오류 메시지를 출력합니다:
    Serial.println("Error opening test.txt");
  }
}

void loop() {
  // 설정 이후 아무 일도 일어나지 않습니다
}
```

### 샘플 출력:
이 코드를 실행하면, 아두이노 IDE 시리얼 모니터에 다음과 같이 표시됩니다:
```
Initialization done.
Writing to test.txt...done.
```
데이터가 올바르게 작성되었는지 확인하기 위해서, 아두이노에서 SD 카드를 제거한 후 컴퓨터에 삽입하여 `test.txt` 파일을 열어 "Testing text file write." 메시지를 확인할 수 있습니다.

보다 고급 파일 작업이나 처리를 요구하는 프로젝트를 위해서는, 추가적인 라이브러리를 탐색하거나 특정 요구 사항에 맞춰 맞춤 함수를 작성하는 것을 고려하세요.
