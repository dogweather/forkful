---
title:                "Arduino: 임시 파일 만들기"
simple_title:         "임시 파일 만들기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 왜

여러분은 아두이노 프로그래밍에 관심이 있나요? 아마도 여러분은 많은 기능을 가진 편리하고 다양한 작업을 할 수 있는 마이크로컨트롤러에 관심이 있을 것입니다. 이 작은 디바이스는 많은 가능성을 열어줍니다. 오늘 우리는 아두이노에서 임시 파일을 생성하는 방법에 대해 알아보려고 합니다. 임시 파일은 간단한 것처럼 보이지만, 이 작은 개념은 우리에게 매우 유용한 여러 기능을 제공합니다.

## 만드는 방법

아두이노에서 임시 파일을 만드는 방법은 간단합니다. 먼저 파일 시스템을 사용할 수 있도록 "SD" 라이브러리를 가져와야 합니다. 그리고 아두이노 변수에 임시 파일을 생성할 수 있는 함수를 사용하여 파일을 만듭니다. 그런 다음 데이터를 파일에 쓰거나 읽을 수 있습니다. 아래는 임시 파일을 만들고 데이터를 쓰고 읽는 간단한 예제입니다.

```Arduino
#include <SD.h>

File tempFile; // 임시 파일을 저장할 변수
char tempData[] = "Hello world!"; // 임시 파일에 쓸 데이터
int tempDataSize = sizeof(tempData); // 데이터의 크기

void setup(){
  SD.begin(); // SD 카드를 초기화
}

void loop(){
  tempFile = SD.open("temp.txt", FILE_WRITE); // "temp.txt" 파일을 쓰기 모드로 열기
  if (tempFile){ // 파일이 열렸는지 확인
    tempFile.write(tempData, tempDataSize); // 파일에 데이터 쓰기
    tempFile.close(); // 파일 닫기
    tempFile = SD.open("temp.txt"); // "temp.txt" 파일을 읽기 모드로 열기
    if (tempFile){ // 파일이 열렸는지 확인
      String tempString = tempFile.readString(); // 파일에서 문자열 읽기
      Serial.println(tempString); // 시리얼 모니터에 출력
      tempFile.close(); // 파일 닫기
    }
  }
}

```

위의 코드를 작성하고 아두이노 보드에 업로드하면, 시리얼 모니터에 "Hello world!"라는 출력이 나타납니다. 위의 코드에서 "temp.txt"는 임시 파일의 이름이며, 임의로 지정할 수 있습니다. 데이터를 파일에 저장하는 방법도 여러 가지가 있으니, 자신에게 맞게 사용하면 됩니다.

## 깊이 파고들기

임시 파일을 생성하는 것은 우리에게 많은 이점을 제공합니다. 먼저, 우리는 임시 파일로 데이터를 쉽게 저장할 수 있습니다. 그리고 파일 시스템을 사용할 수 있으므로, 여러 데이터를 구조적으로 저장할 수 있고 쉽게 읽고 쓸 수 있습니다. 또한, 임시 파일은 우리의 아두이노 보드의 메모리 공간을 아낄 수 있도록 도와줍니다. 아두이노 보드의 용량이 작은 경우, 임시 파일을 사용해보세요.

## 관련 링크

[SD 라이브러리](https://www.arduino.cc/en/Reference/SD)

[File 객체](https://www.arduino.cc/en/Reference/SDFile)

[자주 묻는 질문 - SD 라이브러리](https://www.arduino.cc/en/Reference/SDFAQ)