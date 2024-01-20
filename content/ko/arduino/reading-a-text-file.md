---
title:                "텍스트 파일 읽기"
html_title:           "Bash: 텍스트 파일 읽기"
simple_title:         "텍스트 파일 읽기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
텍스트 파일을 읽는 것은 데이터를 받아오는 행동입니다. 이는 프로그래머가 어플리케이션에서 사용자 데이터를 불러오고, 처리하기 위해서 필요한 작업입니다.

## 어떻게:
다음은 SD 카드에서 텍스트 파일을 읽는 Arduino 코드 예시입니다.
```Arduino
#include <SD.h>

File myFile;

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    return;
  }
  myFile = SD.open("test.txt");
  if (myFile) {
    while (myFile.available()) {
      Serial.write(myFile.read());
    }
    myFile.close();
  }
}

void loop() {
  // put your main code here, to run repeatedly:
}
```
이 코드는 "test.txt" 텍스트 파일의 데이터를 읽어와 시리얼 모니터에 출력합니다.

## 깊이 들여다 보기:
텍스트 파일을 읽는 행위는 컴퓨터 프로그래밍의 근본적인 가치 중 하나입니다. 과거부터 프로그래머는 텍스트 파일에서 데이터를 읽어와 사용자에게 정보를 제공하거나, 데이터 처리를 수행하기위해 사용하였습니다.

대안으로, 프로그래머는 웹 서버나 데이터베이스로부터 데이터를 불러올 수 있지만, 이는 보통 인터넷 연결이 필요하며, 처리 속도가 상대적으로 느린 경우가 많습니다.

텍스트 파일을 읽는 구현 세부사항은 사용하는 프로그래밍 언어와 플랫폼에 따라 다릅니다. 아두이노 같은 임베디드 시스템에서는 메모리 제한 등의 걸림돌도 존재합니다.

## 참고 자료:
1. Arduino SD 라이브러리: [https://www.arduino.cc/en/reference/SD](https://www.arduino.cc/en/reference/SD)