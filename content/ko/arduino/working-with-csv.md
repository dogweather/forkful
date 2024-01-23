---
title:                "CSV 파일 다루기"
html_title:           "Arduino: CSV 파일 다루기"
simple_title:         "CSV 파일 다루기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (무엇 및 왜?)

CSV(Comma-Separated Values)는 데이터를 쉼표로 구분하는 파일 형식이다. 프로그래머들은 CSV를 사용해 간단하게 데이터를 저장하고 교환하기 위해 사용한다.

## How to: (어떻게:)

아래 예제는 CSV 파일을 읽고 쓰는 방법을 보여준다. 아두이노 SD 라이브러리와 함께 사용한다.

```Arduino
#include <SPI.h>
#include <SD.h>

File myFile;

void setup() {
  Serial.begin(9600);

  if (!SD.begin(10)) {
    Serial.println("SD card failed or not present");
    return;
  }
  
  myFile = SD.open("test.csv", FILE_WRITE);

  if (myFile) {
    myFile.println("id,name,age");
    myFile.println("1,James,45");
    myFile.println("2,Lisa,30");
    myFile.close();
  } else {
    Serial.println("Error opening the file");
  }

  myFile = SD.open("test.csv");
  if (myFile) {
    while (myFile.available()) {
      String data = myFile.readStringUntil('\n');
      Serial.println(data);
    }
    myFile.close();
  } else {
    Serial.println("Error opening the file");
  }
}

void loop() {
  // nothing here
}
```

샘플 출력:
```
id,name,age
1,James,45
2,Lisa,30
```

## Deep Dive (심층 분석)

CSV 형식은 1972년 IBM Fortran (level G) 컴파일러에서 사용된 이래로 데이터를 효율적으로 교환하기 위한 일반적인 방법으로 자리 잡았다. 대안으로는 JSON, XML 등이 있지만, CSV는 가독성과 간단함에서 독보적이다. 아두이노에서 CSV를 다룰 때는 RAM이 제한되어 있으므로 큰 파일을 처리할 때 주의가 필요하다.

## See Also (참조)

- 아두이노 SD 라이브러리 문서: https://www.arduino.cc/en/Reference/SD
- CSV에 관한 더 깊은 이해를 위한 RFC 4180: https://tools.ietf.org/html/rfc4180
- 데이터 포맷 비교 (JSON, XML, CSV): https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
