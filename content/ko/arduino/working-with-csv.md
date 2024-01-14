---
title:                "Arduino: CSV 파일 다루기"
simple_title:         "CSV 파일 다루기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/working-with-csv.md"
---

{{< edit_this_page >}}

# 왜 CSV와 함께 일하나요?

Arduino에 CSV 파일을 사용하는 것은 데이터를 저장하고 관리하기 위한 쉬운 방법입니다. 이를 통해 당신의 프로젝트에서 중요한 데이터를 쉽게 추출할 수 있으며, 확장성과 유연성을 높일 수 있습니다.

# CSV 파일을 사용하는 방법

CSV 파일을 사용하기 위해서는 먼저 "sicsv.h" 라이브러리를 포함해야 합니다. 그런 다음 파일을 읽고 쓰는 함수를 호출하여 데이터를 처리할 수 있습니다. 예를 들어, 다음과 같은 코드를 사용할 수 있습니다.

```Arduino
#include <sicsv.h>

// CSV 파일을 읽기 위한 버퍼를 생성합니다.
char buffer[100];

void setup() {
    // CSV 파일을 읽어올 수 있도록 준비합니다.
    sicsv_init();
    // CSV 파일에서 첫 번째 줄을 읽어옵니다.
    sicsv_read(buffer);
    // 첫 번째 줄의 내용을 시리얼 모니터에 출력합니다.
    Serial.println(buffer);
}

void loop() {
    // 루프를 사용하여 파일의 모든 줄을 읽어옵니다.
    while (sicsv_read(buffer)) {
        // 컴마를 구분 기호로 사용하여 데이터를 나눕니다.
        char *token = strtok(buffer, ",");
        // 각 열의 데이터를 출력합니다.
        while (token != NULL) {
            Serial.println(token);
            token = strtok(NULL, ",");
        }
    }
}
```

위 코드를 실행하면 시리얼 모니터에서 CSV 파일의 내용을 볼 수 있습니다.

# CSV 파일에 대해 깊이 알아보기

CSV 파일은 쉼표로 구분된 값 파일의 약어입니다. 이 파일 형식을 사용하면 엑셀과 같은 프로그램에서 데이터를 쉽게 관리할 수 있습니다. 또한 형식이 간단하므로 여러 프로그래밍 언어에서도 쉽게 처리할 수 있습니다. CSV 파일을 사용하면 데이터베이스나 스프레드시트와 같은 복잡한 시스템을 사용하지 않고도 데이터를 저장하고 관리할 수 있습니다.

# 참고 자료

- [sicsv 라이브러리](https://github.com/funlw65/sicsv)
- [CSV 파일 형식 설명 (영문)](https://www.techopedia.com/definition/8483/comma-separated-values-file-csv)