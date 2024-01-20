---
title:                "텍스트 파일 작성하기"
html_title:           "Arduino: 텍스트 파일 작성하기"
simple_title:         "텍스트 파일 작성하기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
텍스트 파일 작성은 데이터를 영구적으로 저장하는 것입니다. 프로그래머는 로그, 설정, 사용자 데이터 저장을 위해 이를 수행합니다.

## How to: (방법)
```arduino
#include <SD.h>

File myFile;

void setup() {
  Serial.begin(9600);
  SD.begin(10); // SD 카드 모듈 CS 핀이 10번 핀일 때
  myFile = SD.open("example.txt", FILE_WRITE);

  if (myFile) {
    myFile.println("Hello, World!");
    myFile.close();
    Serial.println("Writing done.");
  } else {
    Serial.println("File open failed!");
  }
}

void loop() {
  // 여기서 파일 작성이 필요하지 않으므로 빈 loop 함수를 사용합니다.
}
```
샘플 출력:
```
Writing done.
```

## Deep Dive (심화 학습)
- 역사적 맥락: 초기에는 개인용 컴퓨터들이 작은 데이터를 텍스트로 저장했습니다.
- 대안: EEPROM, 클라우드 저장소, 외부 데이터베이스.
- 구현 세부 사항: `SD.h` 라이브러리를 사용, SPI 통신을 통해 SD 카드와 인터페이스합니다.

## See Also (참조)
- Arduino SD 라이브러리 공식 문서: https://www.arduino.cc/en/Reference/SD
- SPI 통신에 대해: https://www.arduino.cc/en/reference/SPI
- 파일 시스템 및 SD 카드 사용 예제들: https://www.arduino.cc/en/Tutorial/LibraryExamples