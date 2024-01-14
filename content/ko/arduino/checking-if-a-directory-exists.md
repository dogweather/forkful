---
title:                "Arduino: 디렉토리 존재 여부 확인"
simple_title:         "디렉토리 존재 여부 확인"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 왜

디렉토리가 존재하는지 확인하는 것이 중요할 수 있습니다. 예를 들어, 파일을 읽고 쓰는 데 있어서 프로그램이 파일이 있는지 확인해야 할 수 있습니다.

## 설명

디렉토리가 존재하는지 확인하는 방법은 매우 간단합니다. 우선 `SD.begin()` 함수를 사용하여 SD 카드를 초기화해야 합니다. 그런 다음 `SD.exists()` 함수를 사용하여 디렉토리가 존재하는지 확인할 수 있습니다.

```Arduino
#include <SPI.h>
#include <SD.h>

void setup() {
  Serial.begin(9600);
  while (!Serial) {
    ; // 시리얼 포트 대기
  }

  Serial.print("Initializing SD card...");

  if (!SD.begin(4)) { // CS 핀 번호에 따라 다름
    Serial.println("initialization failed!");
    while(1);
  }
  Serial.println("initialization done.");
}

void loop() {
  if (SD.exists("/example_dir")) { // 디렉토리가 있는지 확인
    Serial.println("/example_dir exists.");
  } else {
    Serial.println("/example_dir does not exist.");
  }

  delay(1000);
}
```

예제 출력:

```
Initializing SD card...initialization done.
/example_dir does not exist.
/example_dir exists.
/example_dir exists.
/example_dir exists.
...
```

## 깊이 파헤치기

`SD.exists()` 함수를 사용하여 디렉토리가 존재하는지 확인하는 것은 매우 쉽고 간단한 방법입니다. 하지만 실제로는 해당 디렉토리를 열고 작업을 수행하는 것이 최적의 방법일 수 있습니다.

`SD.exists()` 함수는 `File` 객체를 반환합니다. 이를 사용하여 디렉토리를 열고 내부의 파일을 읽거나 쓸 수 있습니다. 이 방법을 사용하면 디렉토리를 열지 않아도 되므로 성능적인 이점이 있을 수 있습니다.

## 참고 자료

- [Arduino SD 라이브러리 문서](https://www.arduino.cc/en/Reference/SD)
- [SD.begin() 함수 예제](https://www.arduino.cc/en/Reference/SDbegin)
- [SD.exists() 함수 예제](https://www.arduino.cc/en/Reference/SDexists)