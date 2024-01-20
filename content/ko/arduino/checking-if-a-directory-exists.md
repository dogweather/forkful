---
title:                "디렉토리가 존재하는지 확인하기"
html_title:           "Arduino: 디렉토리가 존재하는지 확인하기"
simple_title:         "디렉토리가 존재하는지 확인하기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 무엇과 왜?

디렉터리가 존재하는지 확인하는 것은 프로그래밍에서 폴더나 경로가 실제로 존재하는지를 판단하는 과정입니다. 이는 파일을 저장하거나 경로에서 데이터를 검색할 때 중요한 작업이라고 할 수 있어요.

## 어떻게 할까?

```Arduino
#include <SD.h>

void setup()
{
  Serial.begin(9600);
  if(!SD.begin(4)){
    Serial.println("Card Failure");
    return;
  }
  
  if(SD.exists("/myDirectory")){
    Serial.println("Directory exists.");
  } else {
    Serial.println("Directory does not exist.");
  }
}
```

이 코드를 실행하면 디렉터리가 존재하는지 터미널에 결과가 출력됩니다. 디렉터리가 있으면 "Directory exists."가, 없으면 "Directory does not exist."가 출력됩니다.

## 깊이 파보기

디렉터리 존재 확인은 파일 시스템이 등장한 초기부터 필요했던 기능입니다. 이전에는 파일 시스템 API를 직접 호출하는 방식이 주를 이루었지만, 최근에는 이런 작업을 추상화시킨 라이브러리를 제공하는 경향이 강해지고 있습니다.

Arduino 프로그래밍에서는 SD 라이브러리를 활용해 SD 카드 안에 있는 디렉터리의 존재 여부를 판단할 수 있습니다. 해당 라이브러리를 사용하면 `SD.exists(path)` 함수를 call함으로써 디렉터리나 파일이 존재하는지 쉽게 확인할 수 있습니다.

그 외에도 다양한 방법이 있습니다. 예를 들어, SD 라이브러리 대신 SPIFFS나 LittleFS 라이브러리를 사용해서 ESP8266이나 ESP32의 내부 파일 시스템에서 디렉터리가 존재하는지 확인할 수도 있어요.

## 참고 자료

- [Arduino 공식 SD 라이브러리 문서](https://www.arduino.cc/en/Reference/SD)
- [SD Card Directory Checking Tips](https://forum.arduino.cc/index.php?topic=632377.0)