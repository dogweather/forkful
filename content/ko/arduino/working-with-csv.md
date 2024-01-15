---
title:                "csv 파일 작업하기"
html_title:           "Arduino: csv 파일 작업하기"
simple_title:         "csv 파일 작업하기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/working-with-csv.md"
---

{{< edit_this_page >}}

## 왜

CSV 파일은 데이터를 저장하고 관리하는 데 매우 유용한 형식입니다. 이러한 파일을 사용하면 데이터를 쉽게 불러오고 조작할 수 있으며, 데이터 정리 및 분석에도 매우 유용합니다. Arduino를 사용하여 CSV 파일을 다루는 방법을 배우면 다양한 프로젝트에서 더 나은 데이터 관리를 할 수 있습니다.

## 방법

CSV 파일을 이용하기 위해서는 먼저 해당 파일을 저장할 메모리 공간이 필요합니다. 메모리 공간은 선언문 ```File```을 사용하여 생성할 수 있습니다. 다음 코드를 사용하여 저장한 데이터를 CSV 파일로 출력할 수 있습니다.

```Arduino

#include <SPI.h>
#include <SD.h>

File myFile;

void setup() {
  // SD 카드와 연결
  pinMode(10, OUTPUT);
  if(!SD.begin(4)) {
    return;
  }
  //CSV 파일명
  myFile = SD.open("data.csv", FILE_WRITE);
  // 데이터 입력
  int data = 123;
  // CSV 형식으로 데이터 출력
  myFile.print(data);
  myFile.print(",");
  float data2 = 1.234;
  myFile.println(data2);
  // 파일 닫기
  myFile.close();
}

void loop() {
  // 아무것도 하지 않음
}

```

위 코드를 실행하면 SD 카드에 "data.csv"라는 이름의 파일이 생성됩니다. 파일을 열어보면 데이터가 CSV 형식으로 저장되어 있음을 확인할 수 있습니다.

## 깊은 탐구

CSV 파일을 다루기 위해서는 여러 가지 방법이 있지만, 파일에 데이터를 추가할 때 유의해야 할 점이 있습니다. ```myFile.println()``` 함수를 사용하여 다음 줄에 데이터를 추가하면 데이터가 다음 줄로 넘어가게 됩니다. 이를 해결하기 위해서는 ```myFile.print()``` 함수를 사용하여 데이터를 추가한 뒤, 마지막에 ```myFile.println()``` 함수를 사용하여 줄바꿈을 해주어야 합니다. 또한, 필요한 경우 ```if(myFile.available())``` 함수를 사용하여 파일이 열려있는지 확인한 후 작업을 수행해야 합니다. 이를 통해 프로그램이 비정상적으로 종료되는 상황을 방지할 수 있습니다.

## 참고 자료

- [Arduino SD 라이브러리](https://www.arduino.cc/en/Reference/SD)
- [SD 카드와 상호작용하기](https://www.arduino.cc/en/Tutorial/Datalogger)
- [CSV 파일 형식 분석](https://en.wikipedia.org/wiki/Comma-separated_values)