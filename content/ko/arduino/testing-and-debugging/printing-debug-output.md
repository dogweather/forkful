---
title:                "디버그 출력을 찍어보기"
aliases:
- ko/arduino/printing-debug-output.md
date:                  2024-01-20T17:51:52.602874-07:00
model:                 gpt-4-1106-preview
simple_title:         "디버그 출력을 찍어보기"

tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
프로그래밍에서 디버그 출력은 코드가 어떻게 실행되는지 확인하기 위해 콘솔에 정보를 출력하는 것입니다. 프로그래머들은 버그를 찾고, 코드의 흐름을 이해하기 위해 이 기법을 사용합니다.

## How to: (어떻게 하나요?)
디버그 메시지를 출력하려면, `Serial.begin()`으로 시리얼 통신을 시작하고, `Serial.println()` 또는 `Serial.print()`로 메시지를 출력합니다. 아래는 간단한 예제코드입니다.

```Arduino
void setup() {
  // 시작 시 시리얼 통신을 초기화합니다.
  Serial.begin(9600);
}

void loop() {
  // "Hello, Debug!"라는 메시지를 콘솔에 출력합니다.
  Serial.println("Hello, Debug!");

  // 1초 간격으로 반복
  delay(1000);
}
```

이 코드의 출력 예시:
```
Hello, Debug!
Hello, Debug!
Hello, Debug!
...
```

## Deep Dive (심층 분석)
디버그 출력은 마이크로컨트롤러가 처음 등장했을 때부터 개발자들에게 필수적인 툴이었습니다. 역사적으로, 광범위한 디버깅 도구가 없던 시절에는 콘솔 출력이 거의 유일한 방법이었습니다. 현재 시리얼 모니터링 외에도 LED 깜박임이나 LCD 디스플레이를 사용한 피드백 방법도 있지만, 실시간으로 데이터를 확인할 수 있는 시리얼 출력이 가장 효과적인 경우가 많습니다. 구현 세부 사항으로는 시리얼 통신 속도(baud rate) 설정이 중요하며, 개발하는 보드와 컴퓨터의 시리얼 통신 속도를 일치시켜야 합니다.

## See Also (더 보기)
- [Arduino 공식 웹사이트의 시리얼 문서](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [디버깅 기법 및 팁](https://www.arduino.cc/en/Tutorial/BuiltInExamples/SerialEvent)
- [Arduino IDE 사용법](https://www.arduino.cc/en/Guide/Environment)
