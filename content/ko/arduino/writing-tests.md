---
title:                "테스트 작성하기"
date:                  2024-01-19
html_title:           "Arduino: 테스트 작성하기"
simple_title:         "테스트 작성하기"

category:             "Arduino"
tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
테스트 코드 작성은 프로그램이 원하는 대로 동작하는지 확인하는 과정입니다. 버그를 줄이고, 코드 품질을 높여, 확신을 가지고 제품을 출시할 수 있게 합니다.

## How to: (방법)
Arduino에서는 테스트 프레임워크가 기본적으로 포함되어 있지 않지만, `ArduinoUnit`이나 `AUnit`과 같은 라이브러리를 사용할 수 있습니다. 아래는 `AUnit`을 사용한 간단한 테스트 예제입니다.

```cpp
#include <AUnit.h>

test(ledOnTest) {
  pinMode(13, OUTPUT);
  digitalWrite(13, HIGH);
  assertTrue(digitalRead(13));
}

test(ledOffTest) {
  pinMode(13, OUTPUT);
  digitalWrite(13, LOW);
  assertFalse(digitalRead(13));
}

void setup() {
  Serial.begin(9600);
  aunit::TestRunner::run();
}

void loop() {
}
```

실행 결과, 시리얼 모니터에서 테스트 성공 혹은 실패 결과를 확인할 수 있습니다.

## Deep Dive (심층 분석)
과거에는 Arduino에서의 테스트가 크게 중요하지 않았지만, 제품과 프로젝트가 복잡해짐에 따라 테스트의 중요성이 커지고 있습니다. 다른 제품들과 같이 테스트 가능한 코드를 작성하는 것이 중요하며, `googletest`와 같은 다른 테스팅 프레임워크도 마이크로컨트롤러에 적용하기 위해 변형되고 있습니다. 구현 세부사항에는 테스트 케이스의 정의, 암시적 테스트 설정과 해체 등이 포함됩니다.

## See Also (참고 자료)
- `AUnit` GitHub: https://github.com/bxparks/AUnit
- ArduinoUnit GitHub: https://github.com/mmurdoch/arduinounit
- 테스팅에 대한 일반적인 개념: https://www.arduino.cc/en/Guide/TestEquipment
