---
date: 2024-01-20 17:53:30.875202-07:00
description: "How to: (\uBC29\uBC95) \uC544\uB450\uC774\uB178\uC5D0\uC120 SD \uCE74\
  \uB4DC \uBAA8\uB4C8\uC744 \uC0AC\uC6A9\uD574 \uD14D\uC2A4\uD2B8 \uD30C\uC77C\uC744\
  \ \uC77D\uC744 \uC218 \uC788\uB2E4. \uAC04\uB2E8\uD55C \uC608\uC81C\uB97C \uC0B4\
  \uD3B4\uBCF4\uC790."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:57.272074-06:00'
model: gpt-4-1106-preview
summary: "(\uBC29\uBC95) \uC544\uB450\uC774\uB178\uC5D0\uC120 SD \uCE74\uB4DC \uBAA8\
  \uB4C8\uC744 \uC0AC\uC6A9\uD574 \uD14D\uC2A4\uD2B8 \uD30C\uC77C\uC744 \uC77D\uC744\
  \ \uC218 \uC788\uB2E4."
title: "\uD14D\uC2A4\uD2B8 \uD30C\uC77C \uC77D\uAE30"
weight: 22
---

## How to: (방법)
아두이노에선 SD 카드 모듈을 사용해 텍스트 파일을 읽을 수 있다. 간단한 예제를 살펴보자.

```Arduino
#include <SD.h>
#include <SPI.h>

void setup() {
  Serial.begin(9600);
  while (!Serial) {
    ; // wait for serial port to connect.
  }

  if (!SD.begin(4)) {
    Serial.println("SD card initialization failed!");
    return;
  }
  
  File dataFile = SD.open("example.txt");
  if (dataFile) {
    while (dataFile.available()) {
      Serial.write(dataFile.read());
    }
    dataFile.close();
  } else {
    Serial.println("File open failed!");
  }
}

void loop() {
  // Nothing here
}
```
파일에 있는 내용이 시리얼 모니터에 표시된다.

## Deep Dive (심층 분석)
텍스트 파일 읽기 기능은 아두이노에서 다양한 프로젝트에 활용되며, 역사적으로는 punched tape에서 시작하여 디지털 저장 매체로 발전했다. SD 카드 모듈 사용은 가장 일반적인 방법이지만, EEPROM이나 인터넷을 통한 데이터 접근 방식도 있다. 데이터 형식에 따라 read(), readBytes(), readString() 등 다양한 함수를 사용하여 구현할 수 있다.

## See Also (참고 자료)
- 아두이노 공식 SD 라이브러리 문서: [Arduino - SD](https://www.arduino.cc/en/Reference/SD)
- SPI 라이브러리 소개: [Arduino - SPI](https://www.arduino.cc/en/Reference/SPI)
- EEPROM 읽기와 쓰기: [Arduino - EEPROMRead](https://www.arduino.cc/en/Tutorial/EEPROMRead)
