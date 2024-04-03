---
date: 2024-01-26 01:09:05.292494-07:00
description: "\uC5B4\uB5BB\uAC8C \uD560\uAE4C? LED\uB97C \uAE5C\uBE61\uC774\uACE0\
  \ \uC2F6\uB2E4\uACE0 \uC0C1\uC0C1\uD574 \uBCF4\uC138\uC694. \uD568\uC218\uAC00 \uC5C6\
  \uB2E4\uBA74 `loop`\uB294 \uC5B4\uC9C8\uB7EC\uC9C4 \uD63C\uB780\uC785\uB2C8\uB2E4\
  . \uD568\uC218\uAC00 \uC788\uB2E4\uBA74 \uB9D0\uB054\uD569\uB2C8\uB2E4. \uC774\uB807\
  \uAC8C \uD558\uBA74 \uB429\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.615535-06:00'
model: gpt-4-1106-preview
summary: "LED\uB97C \uAE5C\uBE61\uC774\uACE0 \uC2F6\uB2E4\uACE0 \uC0C1\uC0C1\uD574\
  \ \uBCF4\uC138\uC694."
title: "\uCF54\uB4DC\uB97C \uD568\uC218\uB85C \uAD6C\uC131\uD558\uAE30"
weight: 18
---

## 어떻게 할까?
LED를 깜빡이고 싶다고 상상해 보세요. 함수가 없다면 `loop`는 어질러진 혼란입니다. 함수가 있다면 말끔합니다. 이렇게 하면 됩니다:

```Arduino
const int LED_PIN = 13;

void setup() {
  pinMode(LED_PIN, OUTPUT);
}

void loop() {
  blinkLED(500); // LED를 500ms마다 깜빡임
}

// LED를 깜빡이게 하는 함수
void blinkLED(int delayTime) {
  digitalWrite(LED_PIN, HIGH);
  delay(delayTime);
  digitalWrite(LED_PIN, LOW);
  delay(delayTime);
}
```

샘플 출력: LED가 즐겁게 깜빡이고, 코드의 목적이 한눈에 명확합니다.

## 심층 분석
함수가 있기 전의 프로그래밍은 선형 도로 여행과 같았으며, 시작부터 끝까지 모든 포트홀을 봐야 했습니다. 함수가 생긴 후에는 중요한 부분으로 바로 넘어가는 비행에 더 가깝습니다. 역사적으로, 서브루틴(초기의 함수)은 프로그래밍에서 혁명이었으며, 프로그래머들이 반복적인 작업을 피할 수 있게 해주었습니다 - 그것이 DRY 원칙, 즉 '반복하지 말라(Do not Repeat Yourself)'입니다. 함수의 대안으로는 매크로나 객체 지향 프로그래밍(OOP)을 위한 클래스 사용이 포함될 수 있습니다. 요점인 즉슨, 함수를 정의할 때, 작업을 수행하기 위한 컴파일러용 청사진을 제공하는 것입니다. Arduino와 함께하는 경우, 대부분 마이크로컨트롤러를 위한 간단한 명령으로 작용하는 void 함수를 정의하게 되지만, 함수는 값도 반환할 수 있어 더 다양한 용도로 쓰일 수 있습니다.

## 또한 보기
함수에 대해 더 알고 싶다면 이것들을 참고하세요:

- Arduino의 공식 함수 참조: https://www.arduino.cc/reference/en/language/functions/
- DRY 원칙에 대해 더 알아보기: https://en.wikipedia.org/wiki/Don%27t_repeat_yourself
- 서브루틴의 역사에 대한 복습: https://en.wikipedia.org/wiki/Subroutine
