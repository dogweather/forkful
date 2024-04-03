---
date: 2024-01-20 17:39:47.706153-07:00
description: "\uC784\uC2DC \uD30C\uC77C \uC0DD\uC131\uC740 \uB370\uC774\uD130\uB97C\
  \ \uC77C\uC2DC\uC801\uC73C\uB85C \uC800\uC7A5\uD558\uAE30 \uC704\uD55C \uD30C\uC77C\
  \uC744 \uB9CC\uB4DC\uB294 \uAC83\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\
  \uB4E4\uC740 \uC2DC\uC2A4\uD15C\uC774 \uB044\uAE30 \uC804\uC5D0 \uB370\uC774\uD130\
  \uB97C \uC783\uC9C0 \uC54A\uB3C4\uB85D, \uD639\uC740 \uB3C5\uB9BD\uC801\uC778 \uB370\
  \uC774\uD130 \uC870\uC791\uC744 \uC704\uD574 \uC774 \uBC29\uBC95\uC744 \uC0AC\uC6A9\
  \uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.636534-06:00'
model: gpt-4-1106-preview
summary: "\uC784\uC2DC \uD30C\uC77C \uC0DD\uC131\uC740 \uB370\uC774\uD130\uB97C \uC77C\
  \uC2DC\uC801\uC73C\uB85C \uC800\uC7A5\uD558\uAE30 \uC704\uD55C \uD30C\uC77C\uC744\
  \ \uB9CC\uB4DC\uB294 \uAC83\uC785\uB2C8\uB2E4."
title: "\uC784\uC2DC \uD30C\uC77C \uC0DD\uC131\uD558\uAE30"
weight: 21
---

## What & Why? (무엇과 왜?)
임시 파일 생성은 데이터를 일시적으로 저장하기 위한 파일을 만드는 것입니다. 프로그래머들은 시스템이 끄기 전에 데이터를 잃지 않도록, 혹은 독립적인 데이터 조작을 위해 이 방법을 사용합니다.

## How to: (어떻게 하나요?)
Arduino에는 표준 파일 시스템이 없어서 직접적인 임시 파일 생성 예시는 제공할 수 없습니다. 하지만, SD 카드 모듈을 사용해 파일을 만들고 수정하는 것은 가능합니다. 다음은 SD 카드에 임시 파일을 만드는 방법의 예시입니다.

```Arduino
#include <SPI.h>
#include <SD.h>

File myTempFile;

void setup() {
  Serial.begin(9600);
  while (!Serial) {
    ; // 기다리기 위하여 사용합니다.
  }

  Serial.print("Initializing SD card...");
  if (!SD.begin(4)) {
    Serial.println("initialization failed!");
    return;
  }
  Serial.println("initialization done.");

  // 임시 파일 생성
  myTempFile = SD.open("temp.txt", FILE_WRITE);
  if (myTempFile) {
    Serial.println("Writing to temp file...");
    myTempFile.println("Just some temporary data.");
    myTempFile.close(); // 데이터 기록 후 파일 닫기
    Serial.println("Done.");
  } else {
    // 파일 열기 실패
    Serial.println("Error opening temp file.");
  }
}

void loop() {
  // Empty loop
}
```
SD 카드에 'temp.txt'라는 임시 파일을 성공적으로 생성하고 데이터를 기록한 후의 샘플 출력입니다:
```
Initializing SD card...initialization done.
Writing to temp file...
Done.
```

## Deep Dive (자세히 들여다보기)
Arduino는 다양한 프로젝트에서 사용될 수 있어 표준 파일 시스템이 내장되어 있지 않습니다. 대신, 외부 메모리 방식을 사용하여 파일을 관리합니다. SD 카드 모듈을 예로 들 수 있습니다. 파일 시스템과 SD 라이브러리를 이용하면 파일을 열고, 읽고, 쓸 수 있습니다. 'temp.txt'와 같은 임시 파일이 필요하면, 프로그램이 시작할 때 새 파일을 생성하고 끝날 때 파일을 삭제하면 됩니다. 이 방식은 UNIX 시스템의 tmp 폴더를 사용하는 것과는 다르지만, 임시 파일의 개념은 동일합니다. 대안으로, EEPROM을 사용해 임시 데이터를 저장할 수도 있으나 여기에는 아주 제한적인 공간만이 제공됩니다.

## See Also (더 보기)
- Arduino SD 라이브러리 사용 방법: https://www.arduino.cc/en/Reference/SD
- SD 카드와의 파일 작업: https://www.arduino.cc/en/Tutorial/LibraryExamples/ReadWrite
- Arduino EEPROM 사용법: https://www.arduino.cc/en/Reference/EEPROM
