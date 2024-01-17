---
title:                "CSV 작업"
html_title:           "Arduino: CSV 작업"
simple_title:         "CSV 작업"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/working-with-csv.md"
---

{{< edit_this_page >}}

# CSV 파일과 아두이노 코딩: 무엇이고 왜하는가?

작업하는 csv 파일이란 무엇일까요? 이 파일은 쉼표로 구분된 텍스트 데이터이며, 다양한 정보를 가지고 있습니다. 프로그래머들은 이 파일을 사용하여 정보를 쉽게 관리하고 읽고 쓰기 위해 사용합니다.

# 어떻게: 아두이노 코드 블록 내에서 코딩 예제 및 샘플 출력

```Arduino
#include <SPI.h> // SPI 라이브러리 추가
#include <SD.h> // SD 라이브러리 추가
#include <SoftwareSerial.h> // 소프트웨어 시리얼 라이브러리 추가

SoftwareSerial mySerial(2, 3); // 아두이노 핀 2와 3을 통해 소프트웨어 시리얼 생성

File csvFile; // csv 파일 변수 생성
const int chipSelect = 4; // CS 핀 설정
String data = "Sensor 1, Sensor 2"; // 사용할 데이터를 문자열로 저장

void setup() {
  Serial.begin(9600); // 시리얼 통신 시작
  pinMode(chipSelect, OUTPUT); // CS 핀 출력으로 설정
  
  if(!SD.begin(chipSelect)) { // SD 카드 초기화
    Serial.println("Could not initialize SD card."); // 초기화 실패 시 오류 메시지 출력
    return;
  }
  
  mySerial.begin(9600); // 소프트웨어 시리얼 통신 시작
  csvFile = SD.open("sensor_data.csv", FILE_WRITE); // csv 파일 열기
  
  if(csvFile) { // 파일 열기 성공 시
    csvFile.println(data); // 파일에 데이터 쓰기
    csvFile.close(); // 파일 닫기
  } else { // 파일 열기 실패 시
    Serial.println("Error opening data file."); // 오류 메시지 출력
  }
}

void loop() {
  // 아두이노에서 센서 값을 읽어와서 data 변수에 저장
  
  csvFile = SD.open("sensor_data.csv", FILE_WRITE); // 파일 열기
  if(csvFile) { // 파일 여는데 성공했을 때
    csvFile.println(data); // 파일에 데이터 쓰기
    csvFile.close(); // 파일 닫기
  } else { // 파일 여는데 실패했을 때
    Serial.println("Error writing data to file."); // 오류 메시지 출력
  }
  delay(1000); // 데이터 업데이트 주기
}
```
아두이노 코드를 이용하여 csv 파일에 데이터를 읽고 쓰는 방법을 살펴보았습니다. 출력된 결과는 sensor_data.csv 파일에 저장되어 있습니다.

# 깊이 알아보기

csv 파일은 텍스트 데이터를 쉼표로 구분하여 정보를 저장하는 형식으로 먼저 개발되었습니다. 현재에는 다양한 데이터 형식이 존재하지만, 여전히 csv 파일은 널리 사용되고 있습니다.

대안으로는 JSON과 XML 형식이 있습니다. 이들은 더 복잡한 데이터 구조를 지원할 수 있지만, CSV는 단순하고 쉽게 읽고 쓸 수 있는 형식이기 때문에 여전히 많은 프로그래머들이 선호하고 사용하고 있습니다.

csv 파일을 다룰 때 주의할 점은 데이터 레코드가 쉼표로 구분되어야 한다는 것입니다. 만약 데이터 레코드 안에 쉼표가 포함되어 있다면, 적절한 따옴표로 묶어주어야 합니다. 또한 데이터 레코드의 개수가 적거나 많을 때도 처리가 필요할 수 있습니다.

# 더 알아보기

[아두이노 공식 사이트에서 csv 파일 관련 문서](https://www.arduino.cc/en/Tutorial/ReadWrite)를 읽어보세요.

[아두이노 라이브러리 중 csv 라이브러리](https://github.com/adafruit/Adafruit_Microduino_CSV)를 사용해 보세요.