---
title:                "Arduino: HTML 구문 분석"
simple_title:         "HTML 구문 분석"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/parsing-html.md"
---

{{< edit_this_page >}}

## 왜?

HTML 파싱을 배우는 이유는 우리 일상생활에서 가장 보편적인 미디어 형식 중 하나이기 때문입니다. 웹사이트, 모바일 애플리케이션 및 컴퓨터 소프트웨어 등 거의 모든 디지털매체에서 HTML을 사용하고 있습니다. 따라서 HTML을 이해하고 파싱하는 것은 아두이노 프로그래밍을 하는 데 매우 유용합니다.

## 방법

우리는 아두이노를 사용해서 웹사이트의 HTML 코드를 파싱하는 방법을 살펴볼 것입니다. 먼저, HTML을 파싱하기 위해 [ArduinoJson 라이브러리](https://arduinojson.org)를 설치해야 합니다. 아두이노 IDE에서 "툴" 메뉴를 선택한 다음 "라이브러리 관리자"를 클릭하고 "ArduinoJson"을 검색하여 설치할 수 있습니다.

아래는 아두이노를 사용한 HTML 파싱 예제입니다. 코드에서 주석을 참고하여 각 부분이 무엇을 하는지 이해하시기 바랍니다.

```Arduino
#include <ArduinoJson.h> // ArduinoJson 라이브러리를 포함시킵니다.

// 웹사이트에서 다운로드한 HTML 코드를 변수로 저장합니다.
String html = "<!DOCTYPE html><html><body><h1>안녕하세요!</h1><p>이것은 예제입니다.</p></body></html>";

void setup() {
  // Serial 모니터를 활성화합니다.
  Serial.begin(9600);
  
  // ArduinoJson의 DynamicJsonDocument 클래스를 사용하여 JSON 객체를 생성합니다.
  // JSON 객체는 일종의 딕셔너리 또는 맵과 유사한 형식으로 데이터를 저장합니다.
  // 또한 객체 이름과 각 항목의 값이 JSON 형식으로 표현됩니다.
  DynamicJsonDocument json(1024);
  
  // json 객체를 json 변수에 파싱합니다.
  deserializeJson(json, html);
  
  // HTML에서 "h1" 태그의 값을 가져와서 serial 모니터에 출력합니다.
  // 인덱스 번호는 0부터 시작하기 때문에 "h1" 태그는 0번째에 위치합니다.
  String value = json["h1"]; // "안녕하세요!"가 출력됩니다.
  Serial.println(value);
  
  // HTML에서 "p" 태그의 값을 가져와서 serial 모니터에 출력합니다.
  // "p" 태그는 1번째에 위치합니다.
  value = json["p"]; // "이것은 예제입니다."가 출력됩니다.
  Serial.println(value);
}

void loop() {
  // 아무 것도 수행하지 않습니다.
}
```

위의 코드에서 볼 수 있듯이 ArduinoJson 라이브러리는 HTML을 쉽게 파싱할 수 있도록 도와줍니다.

## 그 이상으로 들어가기

HTML 파싱은 복잡한 웹 애플리케이션에서 매우 유용합니다. 예를 들어, 여러분은 아두이노를 사용하여 온도, 습도 및 기타 센서 데이터를 수집하고 웹사이트에 표시할 수 있습니다. 이를 통해 여러분은 아두이노 및 웹사이트를 연동하여 데이터를 모니터링하고 원격으로 제어할 수 있습니다.

HTML 파싱과 관련하여 더 자세한 정보를 알고 싶다면 [ArduinoJson 공식 문서](https://arduinojson.org/v6/example/)를 참조하시기 바랍니다.

## 더 알아보기

- [ArduinoJson 라