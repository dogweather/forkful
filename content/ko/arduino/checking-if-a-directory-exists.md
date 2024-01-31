---
title:                "디렉토리의 존재 여부 확인하기"
date:                  2024-01-19
simple_title:         "디렉토리의 존재 여부 확인하기"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (무엇 그리고 왜?)
디렉토리 존재 확인은 파일 시스템에서 특정 폴더(디렉토리)가 있는지 검사하는 것입니다. 프로그래머들은 파일을 저장하거나 읽기 전에 에러를 방지하기 위해 이 작업을 합니다.

## How to: (실행 방법)
Arduino에는 SD 라이브러리가 내장되어 있으며, `SD.exists()` 함수를 사용하여 디렉토리의 존재 여부를 확인할 수 있습니다.

```Arduino
#include <SPI.h>
#include <SD.h>

void setup() {
  Serial.begin(9600);
  while (!Serial) {
    ; // 기다림
  }

  if (!SD.begin(4)) {
    Serial.println("SD 카드 초기화 실패");
    return;
  } else {
    Serial.println("SD 카드 준비 완료");
  }

  if (SD.exists("/example")) {
    Serial.println("/example 디렉토리가 존재합니다.");
  } else {
    Serial.println("/example 디렉토리가 존재하지 않습니다.");
  }
}

void loop() {
  // 여기에 코드를 추가하세요
}
```
샘플 출력:
```
SD 카드 준비 완료
/example 디렉토리가 존재합니다.
```

## Deep Dive (심층 탐구)
SD 라이브러리는 Arduino와 함께 도입되었습니다. 그 전에는 별도의 파일 시스템 라이브러리를 사용해야 했습니다. 대안으로는 SdFat 라이브러리가 있으며, 성능이나 지원되는 파일시스템 형식에서 차이가 날 수 있습니다. `SD.exists()` 함수는 내부적으로 파일 시스템 테이블을 검색하여 디렉토리의 존재 여부를 반환합니다.

## See Also (관련 자료)
- Arduino SD 라이브러리: https://www.arduino.cc/en/Reference/SD
- SdFat 라이브러리: https://github.com/greiman/SdFat
- SPI 라이브러리: https://www.arduino.cc/en/Reference/SPI
