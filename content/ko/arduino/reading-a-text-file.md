---
title:    "Arduino: 텍스트 파일 읽기"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## 왜

텍스트 파일을 읽는 방법에 대한 이해는 Arduino 프로그래밍의 기본적인 개념입니다. 이를 통해 더 많은 현실적인 응용 프로그램을 개발할 수 있으며, 데이터를 저장하고 처리하는 데 있어서도 매우 유용합니다. 

## 방법

```Arduino
/*
 Arduino에서 텍스트 파일 읽기 예제
*/

// SD 카드 라이브러리 가져오기
#include <SD.h> 

// 사용할 핀 정의
// MOSI, MISO, CLK 핀은 SD 카드 쉴드에 이미 연결되어 있음
// 따라서 새로운 핀 번호를 사용해야 함
const int SD_CS_PIN = 10; // CS 핀
const int FILENAME_MAX_LEN = 20; // 읽을 파일 이름의 최대 길이
char filename[20] = "data.txt"; // 읽을 파일의 이름

File myFile; // 파일 객체 생성

void setup() {
  // SD 카드 초기화
  Serial.begin(9600);
  while (!Serial) {
    continue;
  }
  Serial.println("Initializing SD card...");

  // SD 카드에서 파일을 읽기 전에 핀 설정
  pinMode(SD_CS_PIN, OUTPUT);

  // SD 카드 라이브러리 초기화
  if (!SD.begin(SD_CS_PIN)) {
    Serial.println("SD card initialization failed!");
    return;
  }
  Serial.println("SD card initialized successfully!");

  // 텍스트 파일 열기
  myFile = SD.open(filename);

  // 파일을 성공적으로 열었는지 확인
  if (myFile) {
    Serial.println("File opened successfully!");
    
    // 텍스트 파일에서 한 줄씩 읽어서 시리얼 모니터에 출력
    while (myFile.available()) {
      Serial.write(myFile.read());
    }
    
    // 파일을 닫기 전에 시리얼 모니터에 메시지 출력
    Serial.println();
    Serial.println("File read successfully!");
    
    // 파일 닫기
    myFile.close();
  } else {
    // 파일을 열 수 없는 경우 에러 메시지 출력
    Serial.println("Could not open file!");
  }
}

void loop() {
  // 아무 작업도 수행하지 않음
}
```

위의 코드는 SD 카드에서 텍스트 파일을 읽어와 시리얼 모니터에 출력하는 간단한 예제입니다. 코드를 이해하고 해당 디바이스에 적합하게 수정하여 사용할 수 있습니다.

## 딥 다이브

텍스트 파일을 읽는 과정에서 주의해야 할 몇 가지 사항이 있습니다. 첫째, 파일 이름은 반드시 20자 이내여야 합니다. 둘째, 시리얼 모니터에 출력하는 대신 다른 작업을 하도록 코드를 수정할 수 있습니다. 마지막으로, `SD.open()` 함수를 사용하여 파일을 열 때 파일 이름을 문자열로 전달해야 합니다.

## 참고 자료

- [Arduino SD Library Reference](https://www.arduino.cc/en/Reference/SD)
- [Arduino에서 텍스트 파일 읽기](http://www.natekim.com/2014/09/arduino-read-text-file-code.html)