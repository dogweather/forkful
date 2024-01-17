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

## 무엇과 왜?

텍스트 파일 작성이란 무엇인지 알고 계세요? 프로그래머들은 왜 이것을 하게 될까요?

텍스트 파일 작성이란 우리가 사용하는 컴퓨터나 다른 장치에서 사용하기 위해 작성하는 일종의 문서입니다. 이를테면, 우리가 쓰는 간단한 메모장이나 워드 프로세서에서 작성한 문서들이 이에 해당합니다. 프로그래머들은 이 작업을 자주 수행하는데, 왜냐하면 코드나 데이터 등을 저장하기에 매우 유용하기 때문입니다.

## 작성하는 방법:

```Arduino
#include <SPI.h> // SPI 라이브러리

File myFile; // 파일 객체 생성

void setup() {
  // 초기화
  Serial.begin(9600);
  while (!Serial) {
    ; // 시리얼이 연결될 때까지 기다립니다.
  }

  Serial.print("Initializing SD card...");

  // SD 카드 삽입 여부 확인
  if (!SD.begin(4)) {
    Serial.println("SD card failed, or not present");
    // 계속 진행하지 않을 겁니다. something wrong with the SD card
    while (1);
  }
  Serial.println("SD card initialized.");

  // 파일 열기, 읽기/쓰기 모드
  myFile = SD.open("test.txt", FILE_WRITE);

  // 파일이 잘 열렸는지 확인
  if (myFile) {
    // 파일에 문장 쓰기
    Serial.print("Writing to test.txt...");
    myFile.println("This is a test file.");
    myFile.println("You can write anything you want in here.");
    // 파일 닫기
    myFile.close();
    Serial.println("done.");
  } else {
    // 파일이 열리지 않았을 경우, 오류 메시지를 시리얼 모니터에 출력
    Serial.println("error opening test.txt");
  }
}

void loop() {
  // 파일 열기, 읽기 모드
  myFile = SD.open("test.txt");

  // 파일이 제대로 열렸는지 확인
  if (myFile) {
    Serial.println("test.txt:");

    // 파일에 있는 모든 내용 읽기
    while (myFile.available()) {
      Serial.write(myFile.read());
    }
    // 파일 닫기
    myFile.close();
  } else {
    // 파일이 열리지 않았을 경우, 오류 메시지를 시리얼 모니터에 출력
    Serial.println("error opening test.txt");
  }
}
```

## 더 깊이 들어가기:

### 역사적 배경

텍스트 파일 작성은 컴퓨터의 초기에 앞서 자리를 잡았습니다. 그 이후로도 계속해서 자주 사용되는 용도 중 하나이죠. 초기에는 파일 작성을 하기 위해서는 운영체제나 프로그래밍 언어마다 개별적으로 명령어를 입력해야 했습니다. 하지만 지금은 많은 프로그래밍 언어에서 텍스트 파일 작성을 쉽게 할 수 있도록 내장 함수나 라이브러리를 제공하고 있습니다.

### 대안

텍스트 파일 작성을 위해서는 다른 프로그래밍 언어를 사용해도 가능합니다. C++, Java, Python 등 여러 언어에서도 텍스트 파일 작성 기능을 제공하고 있습니다. 그러나 아두이노는 작은 크기의 프로젝트를 위한 간편한 개발 환경을 제공하므로, 텍스트 파일 작성 기능을 사용하기에 적합합니다.

### 구현 세부 사항

프로그래밍에서 파일 작성을 할 때 가장 중요한 것은 새로운 파일을 생성할 때 파일 이름과 확장자를 잘 정하는 것입니다. 파일 이름은 파일의 내용을 잘 대표할 수 있도록 잘 정하는 것이 좋습니다. 또한, 파일 이름에 특수 문자나 공백을 넣으면 오류가 발생할 수 있으므로 가급적이면 사용하지 않는 것이 좋습니다. 또한 아두이노에서는 SD 라이브러리를 사용하여 SD 카드에 접근해서 파일을 작성합니다. 따라서 SD 카드가 연결되어 있는지 확인 후 작성을 시작해야 합니다.

## 같이 보기

- [Arduino SD Library Reference](https://www.arduino.cc/en/reference/SD)
- [Writing a Text File in C++](https://www.w3schools.com/cpp/cpp_files.asp)
- [Python Files and File Handling](https://www.w3schools.com/python/python_file_handling.asp)