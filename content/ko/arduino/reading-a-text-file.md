---
title:    "Arduino: 텍스트 파일 읽기"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 왜

텍스트 파일을 읽는 것이 왜 중요한지 궁금하셨나요? 이 블로그 포스트에서 우리는 Arduino에서 텍스트 파일을 읽는 방법과 그 이유를 자세히 알아보도록 하겠습니다.

## How To

Arduino에서 텍스트 파일을 읽기 위해서는 다음과 같은 단계를 따라야 합니다. 먼저, SD 카드 모듈을 사용하여 Arduino와 연결합니다. 그런 다음 SD 라이브러리를 사용하여 SD 카드에 액세스하여 파일을 열고 데이터를 읽습니다. 아래에 코드 예제와 함께 간단한 텍스트 파일을 읽는 예제를 살펴보겠습니다.

```Arduino
#include <SPI.h>
#include <SD.h>

File myFile;
// SD카드 모듈을 10번 핀에 연결합니다.
const int chipSelect = 10;
// 데이터를 저장할 문자열을 선언합니다.
String data;

void setup() {
  Serial.begin(9600);
  // SD 카드와 연결될 수 있도록 10번 핀을 출력으로 설정합니다.
  pinMode(chipSelect, OUTPUT);
  // SD 카드를 초기화합니다.
  if (!SD.begin(chipSelect)) {
    Serial.println("SD 카드를 찾을 수 없습니다.");
    while (1);
  }
  Serial.println("SD 카드가 활성화되었습니다.");
  // 텍스트 파일을 읽기 위해 "test.txt"를 엽니다.
  myFile = SD.open("test.txt");
  // 파일의 모든 데이터를 읽을 때까지 반복합니다.
  while (myFile.available()) {
    // 파일에서 문자를 읽고 data 변수에 추가합니다.
    data += char(myFile.read());
  }
  Serial.println("파일의 내용:");
  // 읽은 데이터를 시리얼 모니터에 출력합니다.
  Serial.println(data);
  // 파일을 닫습니다.
  myFile.close();
}

void loop() {

}
```

### 예상 출력
```
SD 카드가 활성화되었습니다.
파일의 내용:
Arduino를 배우는 것은 재밌는 일이에요!

```

## Deep Dive
텍스트 파일을 읽는 것은 다양한 프로젝트에서 매우 유용합니다. 만약 온도 센서와 연결되어 있는 Arduino가 있다면, 온도 값을 텍스트 파일로 저장하여 나중에 데이터를 분석할 수 있습니다. 또는 공중화장실의 사용량을 추적하는 시스템을 만들고 싶다면, 각각의 사용 기록을 텍스트 파일로 저장하여 관리할 수 있습니다. 텍스트 파일은 우리가 일상에서 사용하는 모든 회사에서도 중요한 역할을 합니다. 예를 들어, 고객의 정보를 데이터베이스에 저장할 때, 보통 텍스트 파일로 데이터를 먼저 읽은 후 데이터베이스에 저장합니다.

## See Also
- Arduino에서 SD 카드 모듈 사용하기: https://www.arduino.cc/en/Reference/SD
- C++에서 텍스트 파일 읽는 방법: https://www.w3schools.com/cpp/cpp_files.asp
- Arduino로 텍스트 파일에 데이터 저장하기: https://www.instructables.com/id/Arduino-Text-Files/