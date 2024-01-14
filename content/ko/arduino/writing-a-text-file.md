---
title:                "Arduino: 텍스트 파일 작성하기"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 왜

텍스트 파일을 작성하는 이유는 여러 가지가 있을 수 있습니다. 예를 들어, 센서에서 수집한 데이터를 기록하고 분석하기 위해서이거나, 사용자에게 정확한 정보를 표시하기 위해 사용자와 상호 작용하는 프로그램에서 사용될 수도 있습니다.

## 사용 방법

아두이노에서 텍스트 파일을 작성하는 것은 간단한 과정입니다. 먼저, ```SD```라이브러리를 임포트하고 적절한 핀을 설정해야합니다. 다음으로, 사용하고자 하는 파일의 이름과 모드를 정의해야합니다. 파일을 열고 쓰기 위해 ```open()``` 함수와 파일에 내용을 추가하기 위해 ```println()``` 함수를 사용합니다. 아래 예시 코드를 참고하세요.

```Arduino
#include <SD.h>

File myFile;
const int chipSelect = 10;

void setup() {
    Serial.begin(9600);
    while (!Serial) {
        ;
    }

    Serial.print("Initializing SD card...");
    if (!SD.begin(chipSelect)) {
        Serial.println("ERROR - SD card initialization failed!");
        return;
    }
    Serial.println("SUCCESS - SD card initialized.");
    
    // 파일 이름과 모드를 정의합니다.
    myFile = SD.open("test.txt", FILE_WRITE);

    // 파일에 내용을 작성합니다.
    myFile.println("Hello World");
    myFile.println("This is a text file written by Arduino.");

    // 파일을 닫습니다.
    myFile.close();
}

void loop() {
    // 아무것도 하지 않습니다.
}
```

위 코드를 실행하면 아두이노 보드에 연결된 SD 카드에 ```test.txt``` 파일이 생성되고 해당 파일에 내용이 작성됩니다.

## 깊이 파고들기

파일을 열고 작성하는 것 외에도, 아두이노에서는 텍스트 파일을 읽고 삭제하는 등 다양한 작업이 가능합니다. ```open()``` 함수의 다양한 모드를 사용하여 파일을 열 수 있고, ```read()```, ```available()```, ```seek()```와 같은 다양한 함수를 사용하여 파일을 읽거나 조작할 수 있습니다. 더 자세한 정보는 아두이노 공식 문서를 참고하세요.

## 참고

- [Arduino 공식 문서: SD Library](https://www.arduino.cc/en/Reference/SD)
- [Arduino Forum: Writing a text file to the SD card](https://forum.arduino.cc/t/writing-a-text-file-to-the-sd-card/99575)