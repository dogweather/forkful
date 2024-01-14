---
title:                "Arduino: json 작업하기"
simple_title:         "json 작업하기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/working-with-json.md"
---

{{< edit_this_page >}}

# 왜 JSON을 사용하는가

JSON은 데이터를 교환하고 저장하기 위한 유용한 방법이 될 수 있습니다. 또한 Arduino와 같은 임베디드 시스템에서는 작은 용량을 차지하며 다양한 응용 프로그램에 적합하다는 장점이 있습니다.

## 방법

다음은 JSON을 사용하여 간단한 데이터를 보내고 받는 방법에 대한 예시 코드입니다.

```arduino
#include <ArduinoJson.h>

void setup() {
  Serial.begin(9600);
}

void loop() {
  // JSON 데이터 생성
  StaticJsonDocument<200> doc;
  doc["name"] = "John";
  doc["age"] = 25;

  // JSON 데이터를 문자열로 변환하여 전송
  String jsonStr;
  serializeJson(doc, jsonStr);
  Serial.println(jsonStr);

  // 수신된 JSON 데이터를 처리
  String received = Serial.readStringUntil('\n');
  StaticJsonDocument<200> doc2;
  deserializeJson(doc2, received);
  Serial.print("Name: ");
  Serial.println(doc2["name"].as<String>());
  Serial.print("Age: ");
  Serial.println(doc2["age"].as<int>());

  delay(1000);
}
```

위 코드를 실행하면 시리얼 모니터에 다음과 같은 출력이 나타납니다.

```
{"name":"John","age":25}
Name: John
Age: 25
```

이 예시를 통해 JSON을 사용해 데이터를 보내고 받는 과정을 쉽게 이해할 수 있습니다.

## 깊게 파고들기

JSON은 데이터 형식을 지정할 필요가 없으며 가변형 타입이므로 필요한 만큼 다양한 유형의 데이터를 저장할 수 있습니다. 또한 다양한 라이브러리를 제공하므로 다양한 응용 프로그램에 적용할 수 있습니다.

더 자세한 내용은 다음 링크를 참고해주세요.

[ArduinoJson 공식 문서](https://arduinojson.org/)

[ArduinoJson 라이브러리 다운로드](https://github.com/bblanchon/ArduinoJson/releases)

[여러 가지 JSON 라이브러리 비교](https://www.hanshq.net/json-com.html)

# 또 다른 정보들

- [JSON 형식 소개](https://en.wikipedia.org/wiki/JSON)
- [Arduino와 JSON을 연동하는 예제](https://randomnerdtutorials.com/simply-explained-json/)
- [Arduino와 AJAX를 사용해 웹서버에 JSON 데이터 전송](https://howtomechatronics.com/arduino-webserver-ajax-using-jquery/)