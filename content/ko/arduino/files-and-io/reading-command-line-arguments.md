---
title:                "명령줄 인수 읽기"
aliases:
- /ko/arduino/reading-command-line-arguments/
date:                  2024-01-20T17:55:34.838097-07:00
model:                 gpt-4-1106-preview
simple_title:         "명령줄 인수 읽기"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why? (무엇인가요? 왜 사용하나요?)
커맨드 라인 인수를 읽는다는 것은 사용자가 프로그램 실행 시 명령어로 추가 정보를 제공하는 것입니다. 프로그래머들은 설정을 빠르게 변경하거나 프로그램 동작을 조정하기 위해 이를 사용합니다.

## How to: (어떻게 사용하나요?)
Arduino에서는 전통적인 커맨드 라인 인수에 직접 접근할 수 없습니다. 대신, 시리얼 통신을 사용하여 데이터를 전송하고 읽는 방식을 사용합니다. 아래는 시리얼을 통해 데이터를 읽어오는 예시 코드입니다:

```Arduino
void setup() {
  Serial.begin(9600); // 시리얼 통신을 시작합니다. 여기서 9600은 전송 속도(baud rate)입니다.
}

void loop() {
  if (Serial.available() > 0) {
    String command = Serial.readStringUntil('\n'); // 개행 문자까지 읽어옵니다.
    Serial.println("Received: " + command); // 입력받은 커맨드를 출력합니다.
  }
}
```

샘플 출력:
```
Received: ledOn
```

## Deep Dive (깊이 있게 알아보기)
Arduino는 마이크로컨트롤러를 위한 플랫폼으로, 전통적인 컴퓨터 시스템에서 사용되는 커맨드 라인 인수의 개념이 적용되지 않습니다. 하지만 개발자가 시리얼 모니터나 연결된 소프트웨어를 통해 명령을 전송하면, 마이크로컨트롤러가 이를 받아 로직을 처리할 수 있습니다. 다른 플랫폼에서는 `argc`와 `argv` 변수를 통해 인수를 접근하지만, Arduino에서는 Serial 객체를 통한 통신으로 대신합니다. 이 방식은 실시간 데이터 송수신에 활용될 수 있으며, IoT 디바이스와 같은 네트워크 연결된 환경에서 유용합니다.

## See Also (관련 자료)
- Arduino 공식 문서 시리얼 통신: [Serial - Arduino Reference](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- Arduino 시리얼 통신 튜토리얼: [Arduino - Serial](https://www.arduino.cc/en/Tutorial/BuiltInExamples/SerialEvent)
- IoT 시스템 설계를 위한 Arduino: [Arduino IoT Cloud](https://create.arduino.cc/iot/)
