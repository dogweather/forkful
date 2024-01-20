---
title:                "명령줄 인수 읽기"
html_title:           "Arduino: 명령줄 인수 읽기"
simple_title:         "명령줄 인수 읽기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

명령 행 인수 읽기는 프로그램에서 필요한 데이터를 입력하는 방법입니다. 이것은 프로그래머가 사용자로부터 복잡한 입력을 쉽게 수신할 수 있게 해주는 도구입니다.

## 사용 방법: 

Arduino에서는 명령 행 인수를 직접 읽는 것이 어렵습니다만, 시리얼 통신을 통해 이를 대체할 수 있습니다. 아래의 코드는 예시를 보여줍니다:

```arduino
String input;
void setup() {
  Serial.begin(9600);
}

void loop() {
  if (Serial.available() > 0) {
    input = Serial.readStringUntil('\n');
    Serial.print("Received: ");
    Serial.println(input);
  }
}
```

이 코드는 시리얼로 수신된 데이터를 읽어 출력하는 예입니다. 

## 심화 학습

명령 행 인수가 등장한 것은 초기의 컴퓨터 개발 시기로 거슬러 올라갑니다. 하지만 Arduino와 같은 마이크로컨트롤러에서는 명령 행 인수를 직접 처리하는게 어려워, 위에서 보았듯 시리얼 통신 등의 방법이 사용됩니다. 다른 방법으로는 Arduino가 Wifi 또는 Bluetooth를 통해 서버에 접속해서 데이터를 수신하는 방법이 있습니다.

## 추가 정보:

- Arduino 사이트에 있는 [시리얼 통신 가이드](https://www.arduino.cc/en/Tutorial/BuiltInExamples/SerialEvent)
- [Bluetooth를 통한 데이터 수신 가이드](https://www.arduino.cc/en/Guide/ArduinoUnoWiFi)
- 프로그래밍에 대한 좀 더 복잡한 정보는 [여기에서](https://www.arduino.cc/reference/en/) 찾을 수 있습니다.