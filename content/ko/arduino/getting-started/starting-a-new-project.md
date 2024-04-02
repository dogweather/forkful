---
date: 2024-01-20 18:03:01.235936-07:00
description: "\uC0C8 \uD504\uB85C\uC81D\uD2B8 \uC2DC\uC791\uC740 \uBE48 \uC2A4\uCF00\
  \uCE58\uC5D0\uC11C \uC790\uC2E0\uB9CC\uC758 \uCF54\uB4DC\uB97C \uC4F0\uB294 \uAC83\
  \uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC544\uC774\uB514\
  \uC5B4\uB97C \uD604\uC2E4\uB85C \uB9CC\uB4E4\uAC70\uB098 \uC0C8\uB85C\uC6B4 \uAE30\
  \uC220\uC744 \uBC30\uC6B0\uAE30 \uC704\uD574 \uC774\uB97C \uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.609515-06:00'
model: gpt-4-1106-preview
summary: "\uC0C8 \uD504\uB85C\uC81D\uD2B8 \uC2DC\uC791\uC740 \uBE48 \uC2A4\uCF00\uCE58\
  \uC5D0\uC11C \uC790\uC2E0\uB9CC\uC758 \uCF54\uB4DC\uB97C \uC4F0\uB294 \uAC83\uC785\
  \uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC544\uC774\uB514\uC5B4\
  \uB97C \uD604\uC2E4\uB85C \uB9CC\uB4E4\uAC70\uB098 \uC0C8\uB85C\uC6B4 \uAE30\uC220\
  \uC744 \uBC30\uC6B0\uAE30 \uC704\uD574 \uC774\uB97C \uD569\uB2C8\uB2E4."
title: "\uC0C8 \uD504\uB85C\uC81D\uD2B8 \uC2DC\uC791\uD558\uAE30"
weight: 1
---

## What & Why? (무엇과 왜?)
새 프로젝트 시작은 빈 스케치에서 자신만의 코드를 쓰는 것입니다. 프로그래머들은 아이디어를 현실로 만들거나 새로운 기술을 배우기 위해 이를 합니다.

## How to: (어떻게 하나요?)
Arduino IDE에서 새 프로젝트를 시작하세요. 아래는 LED를 깜박이는 간단한 예제입니다.

```arduino
void setup() {
  pinMode(LED_BUILTIN, OUTPUT); // 내장 LED를 출력으로 설정
}

void loop() {
  digitalWrite(LED_BUILTIN, HIGH);   // LED 켜기
  delay(1000);                       // 1초 기다리기
  digitalWrite(LED_BUILTIN, LOW);    // LED 끄기
  delay(1000);                       // 1초 기다리기
}
```
위 코드를 업로드하면 아두이노의 내장 LED가 1초 간격으로 깜빡이게 됩니다.

## Deep Dive (심층 탐구)
아두이노는 2005년 이탈리아에서 교육용으로 시작되어 전 세계 메이커들과 교육자들에게 사랑받는 플랫폼이 되었습니다. 비주얼 스튜디오 코드(VSCode) 와 같은 대안적인 개발 환경이 있지만, Arduino IDE는 간단하고 접근하기 쉬운 인터페이스로 여전히 많은 사람들에게 첫 선택이 됩니다. 이 IDE는 Wiring 프로그래밍 언어를 사용하여, 하드웨어의 복잡성을 감소시키고 프로그래밍을 쉽게 만들었습니다. 신규 프로젝트를 시작할 때 기본적으로 `setup()` 함수와 `loop()` 함수를 사용합니다. `setup()`은 프로그램 시작 시 한 번 실행되고 `loop()`는 주기적으로 반복 실행됩니다.

## See Also (관련자료)
- Arduino 공식 사이트: [https://www.arduino.cc/](https://www.arduino.cc/)
- 프로젝트 예제 및 튜토리얼: [Arduino Project Hub](https://create.arduino.cc/projecthub)
- Arduino 언어 참조: [Arduino Reference](https://www.arduino.cc/reference/en/)
- 개발 환경 설정과 추가 도구 사용: [Arduino IDE 2.0](https://www.arduino.cc/en/software#experimental-software)
