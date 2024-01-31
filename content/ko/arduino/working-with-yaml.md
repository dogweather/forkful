---
title:                "YAML 다루기"
date:                  2024-01-19
html_title:           "Arduino: YAML 다루기"
simple_title:         "YAML 다루기"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (무엇이며 왜?)
YAML은 데이터를 표현하는 간단한 형식입니다. 프로그래머들은 설정 파일, 데이터 교환 등을 위해 YAML을 사용합니다.

## How to:
Arduino에서 YAML 직접 처리는 제한적이나, 아래 예제는 YAML 형식의 문자열을 해석하는 방법을 설명합니다.

```Arduino
#include <ArduinoJson.h>

const char* yaml = 
  "title: 'YAML Example'\n"
  "value: 10\n";

void setup() {
  Serial.begin(9600);
  DynamicJsonDocument doc(1024);
  deserializeJson(doc, yaml);
  Serial.println(doc["title"].as<String>());
  Serial.println(doc["value"].as<int>());
}

void loop() {
  // 필요한 코드 추가
}
```

출력:
```
YAML Example
10
```

## Deep Dive (심층 분석)
YAML은 JSON 보다 읽고 쓰기 쉬운 대안으로 2001년에 등장했습니다. JSON이나 XML과 비교할 때, YAML은 가독성이 높고 간결합니다. Arduino에선 메모리 제약으로 인해 YAML 라이브러리 지원이 제한적이나, JSON 라이브러리를 사용하여 간단한 YAML 데이터를 처리할 수 있습니다.

## See Also (참조)
- ArduinoJson 라이브러리: https://arduinojson.org/
- YAML 공식 사이트: https://yaml.org/
- YAML vs. JSON: https://json2yaml.com/
