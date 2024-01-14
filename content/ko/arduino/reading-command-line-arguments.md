---
title:                "Arduino: 컴퓨터 프로그래밍에서 명령 줄 인수 읽기"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 왜

왜 우리는 커맨드 라인 인자를 읽는 프로그래밍을 배워야 할까요? 커맨드 라인 인자는 프로그램에서 중요한 역할을 합니다. 예를 들어, 우리가 우리의 아두이노 프로그램에 인자를 제공하면 더 나은 유연성과 사용성을 얻을 수 있습니다.

## 어떻게

우리는 아두이노에서 커맨드 라인 인자를 읽는 방법을 살펴볼 것입니다. 먼저, 다음과 같이 코드를 작성합니다.

```Arduino
#include <Arduino.h>

void setup() {
    // LED를 출력으로 설정합니다.
    pinMode(LED_BUILTIN, OUTPUT);
    // 커맨드 라인 인자의 개수를 알 수 있는 시리얼 포트를 엽니다.
    Serial.begin(9600);
    // 시리얼 모니터를 엽니다.
    Serial.println("Hello World!");
}

void loop() {
    // 시리얼 포트를 통해 입력을 받습니다.
    String command = Serial.readString();
    // 입력된 포트가 1인 경우 LED를 켭니다.
    if (command == "1") {
        digitalWrite(LED_BUILTIN, HIGH);
    }
    // 입력된 포트가 0인 경우 LED를 끕니다.
    if (command == "0") {
        digitalWrite(LED_BUILTIN, LOW);
    }
    // 입력된 포트가 다른 값인 경우 메시지를 출력합니다.
    else {
        Serial.println("Please enter '1' or '0'");
    }
}
```

이제 아두이노 보드에 코드를 업로드하고 시리얼 모니터를 열면 "Hello World!"라는 메시지가 출력될 것입니다. 이제 입력창에 "1"이나 "0"을 입력하면 LED가 켜지거나 꺼질 것입니다.

## 깊은 곳

일반적으로 커맨드 라인 인자는 프로그램을 실행할 때 입력할 수 있도록 사용자에게 제공됩니다. 이렇게하면 프로그램이 더 유연하고 다양한 상황에 대응할 수 있습니다. 예를 들어, 프로그램에서 여러 가지 작업을 수행할 수 있도록 커맨드 라인 인자를 사용할 수 있습니다.

## 참고

- [아두이노 시리얼 통신 사용하기](https://www.arduino.cc/en/Serial/ReadString)
- [커맨드 라인 인자에 대해 더 많이 배우기](https://www.learn-c.org/en/Command_Line_Arguments)