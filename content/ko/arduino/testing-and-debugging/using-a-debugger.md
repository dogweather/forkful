---
date: 2024-01-26 03:48:29.735758-07:00
description: "Arduino IDE\uB97C \uC0AC\uC6A9\uD560 \uB54C\uB294 \uC2DC\uB9AC\uC5BC\
  \ \uCD9C\uB825\uC744 \uC0AC\uC6A9\uD558\uC5EC \uB514\uBC84\uAE45\uD560 \uC218 \uC788\
  \uC9C0\uB9CC, \uC774\uB294 \uB3D9\uAD74 \uD0D0\uD5D8\uC5D0 \uC190\uC804\uB4F1\uC744\
  \ \uC0AC\uC6A9\uD558\uB294 \uAC83\uACFC \uBE44\uC2B7\uD569\uB2C8\uB2E4. \uC2E4\uC81C\
  \ \uB514\uBC84\uAE45\uC744 \uC704\uD574\uC11C\uB294 Atmel-ICE \uB514\uBC84\uAC70\
  \uC640 \uAC19\uC774 Arduino \uD658\uACBD\uACFC \uD1B5\uD569\uB418\uB294 \uAC83\uC744\
  \ \uC0AC\uC6A9\uD558\uB294 \uAC83\uC774 \uC88B\uC2B5\uB2C8\uB2E4. \uC2DC\uB9AC\uC5BC\
  \uC744 \uC0AC\uC6A9\uD55C \uC720\uC0AC\u2026"
lastmod: '2024-03-13T22:44:55.613937-06:00'
model: gpt-4-0125-preview
summary: "Arduino IDE\uB97C \uC0AC\uC6A9\uD560 \uB54C\uB294 \uC2DC\uB9AC\uC5BC \uCD9C\
  \uB825\uC744 \uC0AC\uC6A9\uD558\uC5EC \uB514\uBC84\uAE45\uD560 \uC218 \uC788\uC9C0\
  \uB9CC, \uC774\uB294 \uB3D9\uAD74 \uD0D0\uD5D8\uC5D0 \uC190\uC804\uB4F1\uC744 \uC0AC\
  \uC6A9\uD558\uB294 \uAC83\uACFC \uBE44\uC2B7\uD569\uB2C8\uB2E4."
title: "\uB514\uBC84\uAC70 \uC0AC\uC6A9\uD558\uAE30"
weight: 35
---

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
