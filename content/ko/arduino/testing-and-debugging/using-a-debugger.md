---
date: 2024-01-26 03:48:29.735758-07:00
description: "\uB514\uBC84\uAC70\uB294 \uCF54\uB4DC \uC18D \uBC84\uADF8\uB97C \uC9DC\
  \uB0B4\uB294 \uB370 \uB3C4\uC6C0\uC744 \uC8FC\uB294 \uB3C4\uAD6C\uC785\uB2C8\uB2E4\
  . \uCF54\uB4DC \uC2E4\uD589\uC744 \uC77C\uC2DC \uC911\uC9C0\uD558\uACE0, \uC8FC\uBCC0\
  \uC744 \uC0B4\uD3B4\uBCF4\uBA70, \uC2E4\uC81C\uB85C \uBB34\uC2A8 \uC77C\uC774 \uBC8C\
  \uC5B4\uC9C0\uACE0 \uC788\uB294\uC9C0 \uC54C \uC218 \uC788\uAC8C \uD574\uC90D\uB2C8\
  \uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC790\uC2E0\uC758 \uCF54\uB4DC\
  \uB97C \uB2E8\uACC4\uBCC4\uB85C \uC9C4\uD589\uD558\uBA74\uC11C \uBCC0\uC218\uB97C\
  \ \uAC80\uC0AC\uD558\uACE0 \uBB38\uC81C\uAC00 \uBC1C\uC0DD\uD560 \uC218 \uC788\uB294\
  \ \uC704\uCE58\uB97C \uC774\uD574\uD558\uAE30 \uC704\uD574\u2026"
lastmod: '2024-02-25T18:49:52.602313-07:00'
model: gpt-4-0125-preview
summary: "\uB514\uBC84\uAC70\uB294 \uCF54\uB4DC \uC18D \uBC84\uADF8\uB97C \uC9DC\uB0B4\
  \uB294 \uB370 \uB3C4\uC6C0\uC744 \uC8FC\uB294 \uB3C4\uAD6C\uC785\uB2C8\uB2E4. \uCF54\
  \uB4DC \uC2E4\uD589\uC744 \uC77C\uC2DC \uC911\uC9C0\uD558\uACE0, \uC8FC\uBCC0\uC744\
  \ \uC0B4\uD3B4\uBCF4\uBA70, \uC2E4\uC81C\uB85C \uBB34\uC2A8 \uC77C\uC774 \uBC8C\uC5B4\
  \uC9C0\uACE0 \uC788\uB294\uC9C0 \uC54C \uC218 \uC788\uAC8C \uD574\uC90D\uB2C8\uB2E4\
  . \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC790\uC2E0\uC758 \uCF54\uB4DC\uB97C\
  \ \uB2E8\uACC4\uBCC4\uB85C \uC9C4\uD589\uD558\uBA74\uC11C \uBCC0\uC218\uB97C \uAC80\
  \uC0AC\uD558\uACE0 \uBB38\uC81C\uAC00 \uBC1C\uC0DD\uD560 \uC218 \uC788\uB294 \uC704\
  \uCE58\uB97C \uC774\uD574\uD558\uAE30 \uC704\uD574\u2026"
title: "\uB514\uBC84\uAC70 \uC0AC\uC6A9\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇인가 & 왜인가?

디버거는 코드 속 버그를 짜내는 데 도움을 주는 도구입니다. 코드 실행을 일시 중지하고, 주변을 살펴보며, 실제로 무슨 일이 벌어지고 있는지 알 수 있게 해줍니다. 프로그래머들은 자신의 코드를 단계별로 진행하면서 변수를 검사하고 문제가 발생할 수 있는 위치를 이해하기 위해 디버거를 사용합니다.

## 방법:

Arduino IDE를 사용할 때는 시리얼 출력을 사용하여 디버깅할 수 있지만, 이는 동굴 탐험에 손전등을 사용하는 것과 비슷합니다. 실제 디버깅을 위해서는 Atmel-ICE 디버거와 같이 Arduino 환경과 통합되는 것을 사용하는 것이 좋습니다. 시리얼을 사용한 유사 디버깅의 예는 다음과 같습니다:

```Arduino
void setup() {
  Serial.begin(9600);
}
void loop() {
  int sensorValue = analogRead(A0);
  Serial.print("센서 값: ");
  Serial.println(sensorValue);
  // 여기서 512를 기대하고 있지만 0이 나온다면,
  // 센서 연결을 검사할 시간
  delay(1000); // 다시 읽기 전에 1초 동안 대기
}
```
이것을 시리얼 모니터에서 실행하면 센서가 실시간으로 내뱉는 것을 볼 수 있습니다.

## 심층 탐구

디버거가 등장하기 전에는 print 문의 세계였습니다 – 모든 것을 출력하여 일어나는 일을 추측할 수밖에 없었습니다. 프린트를 사용한 디버깅은 여전히 흔하며, 특히 Arduino와 같이 제약된 하드웨어나 간단한 환경에서는 그렇습니다.

Atmel-ICE와 같은 인-서킷 에뮬레이터에 대한 대안으로는 `avr-gdb`와 같은 소프트웨어 디버깅 도구가 있습니다. `avarice`와 짝을 이뤄 GDB와 하드웨어 사이의 다리를 만들 수 있어 칩 위에서 보다 고급 디버깅에 매우 유용합니다.

디버거를 사용하면 특정 지점에서 실행을 중지시킬 수 있는 중단점을 설정할 수 있습니다. 코드를 한 줄씩 진행하고, 메모리, 레지스터, 변수를 검사할 수 있습니다. 이를 통해 암흑 속에서의 막연한 시도 대신 문제를 정확히 지적할 수 있습니다. 디버거를 구현할 때는 환경이 올바르게 설정되어 있는지 확인하세요 - 버전 불일치나 잘못 구성된 도구는 좌절감을 초래할 수 있습니다.

## 참고 자료

더 깊이 탐구하고 싶으신가요? 다음을 참고하세요:
- [Arduino 디버깅 가이드](https://www.arduino.cc/en/Guide/Environment#toc7)
- avr-gdb 설정을 위한 AVR Libc 참조 매뉴얼: [AVR Libc 홈페이지](http://www.nongnu.org/avr-libc/)
