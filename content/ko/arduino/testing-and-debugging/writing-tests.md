---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:55.900667-07:00
description: "\uC544\uB450\uC774\uB178 \uD658\uACBD\uC5D0\uC11C \uD14C\uC2A4\uD2B8\
  \ \uC791\uC131\uC740 \uC544\uB450\uC774\uB178 \uC7A5\uCE58\uC5D0\uC11C \uCF54\uB4DC\
  \uC758 \uAE30\uB2A5\uC744 \uAC80\uC99D\uD558\uB294 \uC790\uB3D9\uD654\uB41C \uD14C\
  \uC2A4\uD2B8\uB97C \uC0DD\uC131\uD558\uB294 \uACFC\uC815\uC744 \uB9D0\uD569\uB2C8\
  \uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC774 \uC774 \uACFC\uC815\uC744 \uC218\
  \uD589\uD558\uB294 \uC774\uC720\uB294 \uCF54\uB4DC\uAC00 \uC608\uC0C1\uB300\uB85C\
  \ \uC791\uB3D9\uD558\uB294\uC9C0 \uD655\uC778\uD558\uACE0, \uBC84\uADF8\uB97C \uC904\
  \uC774\uBA70, \uD2B9\uD788 \uB514\uBC84\uAE45\uC774 \uB354 \uC5B4\uB824\uC6B8 \uC218\
  \ \uC788\uB294 \uC784\uBCA0\uB514\uB4DC \uC2DC\uC2A4\uD15C\uC5D0\uC11C \uD504\uB85C\
  \uC81D\uD2B8\uC758\u2026"
lastmod: '2024-03-13T22:44:55.612307-06:00'
model: gpt-4-0125-preview
summary: "\uC544\uB450\uC774\uB178 \uD658\uACBD\uC5D0\uC11C \uD14C\uC2A4\uD2B8 \uC791\
  \uC131\uC740 \uC544\uB450\uC774\uB178 \uC7A5\uCE58\uC5D0\uC11C \uCF54\uB4DC\uC758\
  \ \uAE30\uB2A5\uC744 \uAC80\uC99D\uD558\uB294 \uC790\uB3D9\uD654\uB41C \uD14C\uC2A4\
  \uD2B8\uB97C \uC0DD\uC131\uD558\uB294 \uACFC\uC815\uC744 \uB9D0\uD569\uB2C8\uB2E4\
  ."
title: "\uD14C\uC2A4\uD2B8 \uC791\uC131\uD558\uAE30"
weight: 36
---

## 방법:
아두이노는 다른 몇몇 프로그래밍 환경과는 달리 내장된 테스트 프레임워크가 없습니다. 하지만, `AUnit`과 같은 타사 라이브러리를 사용하여 아두이노 코드의 단위 테스트를 수행할 수 있습니다. AUnit은 아두이노의 내장 라이브러리인 `ArduinoUnit`과 구글의 테스트 프레임워크인 `Google Test`에서 영감을 받았습니다.

### AUnit 예제:
먼저, 아두이노 IDE의 라이브러리 관리자를 통해 AUnit을 설치하세요: 스케치 > 라이브러리 포함하기 > 라이브러리 관리... > AUnit을 검색하고 설치합니다.

그런 다음, 다음과 같이 테스트를 작성할 수 있습니다:

```cpp
#include <AUnit.h>

test(ledPinHigh) {
  const int ledPin = 13;
  pinMode(ledPin, OUTPUT);
  digitalWrite(ledPin, HIGH);
  assertTrue(digitalRead(ledPin));
}

test(ledPinLow) {
  const int ledPin = 13;
  pinMode(ledPin, OUTPUT);
  digitalWrite(ledPin, LOW);
  assertFalse(digitalRead(ledPin));
}

void setup() {
  Serial.begin(9600);
  aunit::TestRunner::run();
}

void loop() {
  // 빈 공간
}
```
이 테스트를 아두이노 보드에 업로드 한 후, 시리얼 모니터를 열어 테스트 결과를 확인합니다. 각 테스트가 통과했는지 실패했는지를 나타내는 출력을 볼 수 있습니다:

```
TestRunner가 2개의 테스트를 시작합니다.
Test ledPinHigh가 통과되었습니다.
Test ledPinLow가 통과되었습니다.
TestRunner 지속 시간: 0.002초.
TestRunner 요약: 2 통과, 0 실패, 0 건너뜀, 0 시간 초과, 총 2개의 테스트 중.
```

이 간단한 예제는 LED 핀의 상태를 테스트하기 위해 AUnit을 사용하는 방법을 보여줍니다. 테스트를 생성함으로써, 아두이노가 다양한 조건에서 예상대로 동작하는지 확인할 수 있습니다. AUnit을 사용하면 더 복잡한 테스트, 테스트 스위트를 작성하고, 테스트 타임아웃과 더 진보한 시나리오를 위한 설정/해제 절차와 같은 기능을 이용할 수 있습니다.
