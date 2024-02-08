---
title:                "표준 에러에 쓰기"
aliases:
- ko/arduino/writing-to-standard-error.md
date:                  2024-02-03T19:32:44.934967-07:00
model:                 gpt-4-0125-preview
simple_title:         "표준 에러에 쓰기"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?

아두이노 프로그래밍에서 표준 오류(stderr)로 쓰기는 오류 메시지와 진단을 표준 출력(stdout)과 섞이지 않도록 별도의 채널로 전달하는 것을 포함합니다. 프로그래머들은 이를 통해 일반 프로그램 출력과 오류 메시지를 구분하여 디버깅과 로그 분석을 더욱 간단하게 만듭니다.

## 어떻게:

아두이노는 기존 컴퓨팅 시스템처럼 표준 출력과 표준 오류를 구별하는 기능이 기본적으로 없습니다. `Serial.print()` 및 `Serial.println()` 메서드 모두 동일한 시리얼 출력에 쓰고, 이는 일반적으로 아두이노 IDE 시리얼 모니터에서 볼 수 있습니다. 그러나 오류 메시지를 특별히 포맷하거나 SD 카드나 네트워크 연결과 같은 대안적 출력으로 전달함으로써 stderr로 쓰는 것을 모방할 수 있습니다.

stderr를 모방하기 위해 시리얼 모니터에서 오류 메시지를 구분할 수 있도록 "ERROR:"와 같은 태그를 오류 메시지 앞에 붙일 수 있습니다:

```cpp
void setup() {
  Serial.begin(9600); // 9600 보율에서 시리얼 통신 초기화
}

void loop() {
  int result = someFunction();
  if (result == -1) {
    // 오류 메시지 앞에 접두사를 추가하여 stderr를 모방
    Serial.println("ERROR: 함수가 실행에 실패했습니다.");
  } else {
    Serial.println("함수가 성공적으로 실행되었습니다.");
  }
  delay(1000); // 루프 재시작 전에 1초 기다림
}

int someFunction() {
  // 오류 시 -1을 반환하는 더미 함수
  return -1;
}
```

아두이노 IDE 시리얼 모니터에서의 샘플 출력은 다음과 같습니다:

```
ERROR: 함수가 실행에 실패했습니다.
```

더 복잡한 접근이 필요한 프로젝트의 경우, 다른 물리적 출력으로 쓰기를 포함하여 제3의 라이브러리 사용이나 추가 하드웨어가 필요할 수 있습니다. 예를 들어, 오류 메시지를 SD 카드에 로깅하려면 `SD` 라이브러리가 필요합니다:

```cpp
#include <SPI.h>
#include <SD.h>

File myFile;

void setup() {
  Serial.begin(9600);
  if (!SD.begin()) {
    Serial.println("ERROR: SD 카드 초기화 실패!");
    return;
  }
  
  myFile = SD.open("error.log", FILE_WRITE);
  if (myFile) {
    myFile.println("ERROR: 함수가 실행에 실패했습니다.");
    myFile.close(); // 파일의 내용을 저장하기 위해 반드시 파일을 닫아야 함
  } else {
    Serial.println("ERROR: error.log 열기가 실패했습니다!");
  }
}

void loop() {
  // 여기에 주 코드를 넣을 것
}
```

이 접근 방식을 사용하면, `error.log` 파일로 오류 메시지를 SD 카드에 직접 전달해 일반 프로그램 출력과 오류 메시지를 물리적으로 분리하여, 주 출력 채널을 어지럽히지 않고 사후 분석을 가능하게 합니다.
