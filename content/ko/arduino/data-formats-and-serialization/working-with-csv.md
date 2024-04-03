---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:39.346424-07:00
description: "\uBC29\uBC95: \uC544\uB450\uC774\uB178\uC5D0\uB294 CSV \uD30C\uC77C\uC744\
  \ \uCC98\uB9AC\uD558\uAE30 \uC704\uD55C \uB0B4\uC7A5 \uB77C\uC774\uBE0C\uB7EC\uB9AC\
  \uAC00 \uC5C6\uC9C0\uB9CC, SD \uCE74\uB4DC\uC5D0 \uC788\uB294 \uD30C\uC77C\uC5D0\
  \ \uC811\uADFC\uD558\uAE30 \uC704\uD574 `SD`\uC640 `SPI` \uB77C\uC774\uBE0C\uB7EC\
  \uB9AC\uB97C \uC0AC\uC6A9\uD558\uACE0, \uAE30\uBCF8 \uBB38\uC790\uC5F4 \uC870\uC791\
  \ \uAE30\uC220\uC744 \uC0AC\uC6A9\uD558\uC5EC CSV \uB370\uC774\uD130\uB97C \uD30C\
  \uC2F1\uD558\uAC70\uB098 \uC0DD\uC131\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4. \uBCF4\
  \uB2E4 \uBCF5\uC7A1\uD55C CSV \uC870\uC791\uC744 \uB2E4\uB8F0\u2026"
lastmod: '2024-03-13T22:44:55.641137-06:00'
model: gpt-4-0125-preview
summary: "\uC544\uB450\uC774\uB178\uC5D0\uB294 CSV \uD30C\uC77C\uC744 \uCC98\uB9AC\
  \uD558\uAE30 \uC704\uD55C \uB0B4\uC7A5 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uAC00 \uC5C6\
  \uC9C0\uB9CC, SD \uCE74\uB4DC\uC5D0 \uC788\uB294 \uD30C\uC77C\uC5D0 \uC811\uADFC\
  \uD558\uAE30 \uC704\uD574 `SD`\uC640 `SPI` \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C\
  \ \uC0AC\uC6A9\uD558\uACE0, \uAE30\uBCF8 \uBB38\uC790\uC5F4 \uC870\uC791 \uAE30\uC220\
  \uC744 \uC0AC\uC6A9\uD558\uC5EC CSV \uB370\uC774\uD130\uB97C \uD30C\uC2F1\uD558\uAC70\
  \uB098 \uC0DD\uC131\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "CSV\uC640 \uD568\uAED8 \uC791\uC5C5\uD558\uAE30"
weight: 37
---

## 방법:
아두이노에는 CSV 파일을 처리하기 위한 내장 라이브러리가 없지만, SD 카드에 있는 파일에 접근하기 위해 `SD`와 `SPI` 라이브러리를 사용하고, 기본 문자열 조작 기술을 사용하여 CSV 데이터를 파싱하거나 생성할 수 있습니다. 보다 복잡한 CSV 조작을 다룰 때는 제3자 라이브러리인 `ArduinoCSV`를 사용하면 파싱과 쓰기가 더 쉬워집니다.

**SD 카드에서 CSV 데이터 읽기:**
```cpp
#include <SPI.h>
#include <SD.h>

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("초기화 실패!");
    return;
  }
  File dataFile = SD.open("data.csv");
  if (dataFile) {
    while (dataFile.available()) {
      String dataLine = dataFile.readStringUntil('\n');
      Serial.println(dataLine); // CSV 줄 출력
    }
    dataFile.close();
  } else {
    Serial.println("data.csv 파일을 여는 데 실패했습니다.");
  }
}

void loop() {
  // 이 예제에서는 사용되지 않음
}
```
*출력 예시:*
```
SensorID, Timestamp, Value
1, 1597840923, 23.5
2, 1597840987, 22.4
```

**SD 카드에 CSV 데이터 쓰기:**
```cpp
#include <SPI.h>
#include <SD.h>

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("초기화 실패!");
    return;
  }
  File dataFile = SD.open("output.csv", FILE_WRITE);
  if (dataFile) {
    dataFile.println("SensorID, Timestamp, Value"); // CSV 헤더
    dataFile.println("1, 1597840923, 23.5"); // 예제 데이터 행
    dataFile.close();
    Serial.println("데이터 쓰기 완료");
  } else {
    Serial.println("output.csv 파일을 여는 데 실패했습니다.");
  }
}

void loop() {
  // 이 예제에서는 사용되지 않음
}
```
*출력 예시:*
```
데이터 쓰기 완료
```

**파싱을 위해 ArduinoCSV 사용하기:**
복잡한 CSV 파일을 다루려면 `ArduinoCSV` 라이브러리를 사용하면 파싱 작업이 크게 단순화될 수 있습니다. 이 예제는 이미 `ArduinoCSV` 라이브러리를 설치했다고 가정합니다.

```cpp
#include <SPI.h>
#include <SD.h>
#include <ArduinoCSV.h>

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("초기화 실패!");
    return;
  }
  File dataFile = SD.open("data.csv");
  if (dataFile) {
    CSVParser parser;
    while (dataFile.available()) {
      String dataLine = dataFile.readStringUntil('\n');
      if (parser.parseLine(dataLine)) {
        for (int i = 0; i < parser.count(); i++) {
          Serial.print(parser.getField(i)); // 각 필드 출력
          if (i < parser.count() - 1) {
            Serial.print(", ");
          }
        }
        Serial.println();
      }
    }
    dataFile.close();
  } else {
    Serial.println("data.csv 파일을 여는 데 실패했습니다.");
  }
}

void loop() {
  // 이 예제에서는 사용되지 않음
}
```
*출력 예시:*
```
SensorID,  Timestamp,  Value
1,  1597840923,  23.5
2,  1597840987,  22.4
```
이 예제에서 보듯이, SD 카드에 있는 CSV 파일을 읽고 쓰는 것으로 아두이노 프로젝트는 데이터 수집, 구성 설정 저장, 또는 전 세계적으로 접근 가능한 형식으로 다른 애플리케이션과 데이터를 교환할 수 있습니다.
