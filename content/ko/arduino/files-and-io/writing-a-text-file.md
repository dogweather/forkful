---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:07.238092-07:00
description: "\uC544\uB450\uC774\uB178\uC5D0\uC11C \uD14D\uC2A4\uD2B8 \uD30C\uC77C\
  \uC744 \uC791\uC131\uD55C\uB2E4\uB294 \uAC83\uC740 SD \uCE74\uB4DC\uB098 \uBE44\uC2B7\
  \uD55C \uC800\uC7A5 \uBAA8\uB4C8\uC5D0 \uB370\uC774\uD130\uB97C \uD30C\uC77C\uB85C\
  \ \uC800\uC7A5\uD558\uB294 \uAC83\uC744 \uB9D0\uD558\uBA70, \uC885\uC885 \uB370\uC774\
  \uD130 \uB85C\uAE45 \uBAA9\uC801\uC73C\uB85C \uC0AC\uC6A9\uB429\uB2C8\uB2E4. \uD504\
  \uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC13C\uC11C \uC77D\uAE30 \uAC12\uC744 \uAE30\
  \uB85D\uD558\uAC70\uB098, \uAD6C\uC131\uC744 \uC800\uC7A5\uD558\uAC70\uB098, \uC2DC\
  \uAC04\uC774 \uC9C0\uB0A8\uC5D0 \uB530\uB77C \uC560\uD50C\uB9AC\uCF00\uC774\uC158\
  \ \uC774\uBCA4\uD2B8\uB97C \uAE30\uB85D\uD558\uAE30 \uC704\uD574 \uC774\u2026"
lastmod: '2024-03-13T22:44:55.635058-06:00'
model: gpt-4-0125-preview
summary: "\uC544\uB450\uC774\uB178\uC5D0\uC11C \uD14D\uC2A4\uD2B8 \uD30C\uC77C\uC744\
  \ \uC791\uC131\uD55C\uB2E4\uB294 \uAC83\uC740 SD \uCE74\uB4DC\uB098 \uBE44\uC2B7\
  \uD55C \uC800\uC7A5 \uBAA8\uB4C8\uC5D0 \uB370\uC774\uD130\uB97C \uD30C\uC77C\uB85C\
  \ \uC800\uC7A5\uD558\uB294 \uAC83\uC744 \uB9D0\uD558\uBA70, \uC885\uC885 \uB370\uC774\
  \uD130 \uB85C\uAE45 \uBAA9\uC801\uC73C\uB85C \uC0AC\uC6A9\uB429\uB2C8\uB2E4. \uD504\
  \uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC13C\uC11C \uC77D\uAE30 \uAC12\uC744 \uAE30\
  \uB85D\uD558\uAC70\uB098, \uAD6C\uC131\uC744 \uC800\uC7A5\uD558\uAC70\uB098, \uC2DC\
  \uAC04\uC774 \uC9C0\uB0A8\uC5D0 \uB530\uB77C \uC560\uD50C\uB9AC\uCF00\uC774\uC158\
  \ \uC774\uBCA4\uD2B8\uB97C \uAE30\uB85D\uD558\uAE30 \uC704\uD574 \uC774\u2026"
title: "\uD14D\uC2A4\uD2B8 \uD30C\uC77C \uC4F0\uAE30"
weight: 24
---

## 무엇을, 왜?
아두이노에서 텍스트 파일을 작성한다는 것은 SD 카드나 비슷한 저장 모듈에 데이터를 파일로 저장하는 것을 말하며, 종종 데이터 로깅 목적으로 사용됩니다. 프로그래머들은 센서 읽기 값을 기록하거나, 구성을 저장하거나, 시간이 지남에 따라 애플리케이션 이벤트를 기록하기 위해 이 작업을 수행합니다. 이는 데이터 분석이나 추적을 요구하는 프로젝트에 있어 중요합니다.

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
