---
date: 2024-01-26 01:09:05.292494-07:00
description: "\uCF54\uB4DC\uB97C \uD568\uC218\uB85C \uAD6C\uC131\uD55C\uB2E4\uB294\
  \ \uAC83\uC740 \uCF54\uB4DC\uB97C \uD2B9\uC815 \uC791\uC5C5\uC744 \uC218\uD589\uD558\
  \uB294 \uC7AC\uC0AC\uC6A9 \uAC00\uB2A5\uD55C \uB369\uC5B4\uB9AC\uB85C \uB098\uB204\
  \uB294 \uAC83\uC744 \uC758\uBBF8\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\
  \uB4E4\uC740 \uCF54\uB4DC\uB97C \uC77D\uACE0, \uB514\uBC84\uAE45\uD558\uBA70, \uC7AC\
  \uC0AC\uC6A9\uD558\uAE30 \uC27D\uAC8C \uB9CC\uB4E4\uAE30 \uC704\uD574\uC11C \uC774\
  \uB807\uAC8C \uD569\uB2C8\uB2E4. \uB808\uACE0\uB97C \uC0C1\uC790\uC5D0 \uBD84\uB958\
  \uD558\uB294 \uAC83\uACFC \uAC19\uC544\uC11C, \uBB34\uC5B8\uAC00\uB97C \uB9CC\uB4E4\
  \uACE0 \uC2F6\uC744 \uB54C\uB9C8\uB2E4 \uD63C\uB3C8\uC2A4\uB7EC\uC6B4\u2026"
lastmod: '2024-03-11T00:14:29.541802-06:00'
model: gpt-4-1106-preview
summary: "\uCF54\uB4DC\uB97C \uD568\uC218\uB85C \uAD6C\uC131\uD55C\uB2E4\uB294 \uAC83\
  \uC740 \uCF54\uB4DC\uB97C \uD2B9\uC815 \uC791\uC5C5\uC744 \uC218\uD589\uD558\uB294\
  \ \uC7AC\uC0AC\uC6A9 \uAC00\uB2A5\uD55C \uB369\uC5B4\uB9AC\uB85C \uB098\uB204\uB294\
  \ \uAC83\uC744 \uC758\uBBF8\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\
  \uC740 \uCF54\uB4DC\uB97C \uC77D\uACE0, \uB514\uBC84\uAE45\uD558\uBA70, \uC7AC\uC0AC\
  \uC6A9\uD558\uAE30 \uC27D\uAC8C \uB9CC\uB4E4\uAE30 \uC704\uD574\uC11C \uC774\uB807\
  \uAC8C \uD569\uB2C8\uB2E4. \uB808\uACE0\uB97C \uC0C1\uC790\uC5D0 \uBD84\uB958\uD558\
  \uB294 \uAC83\uACFC \uAC19\uC544\uC11C, \uBB34\uC5B8\uAC00\uB97C \uB9CC\uB4E4\uACE0\
  \ \uC2F6\uC744 \uB54C\uB9C8\uB2E4 \uD63C\uB3C8\uC2A4\uB7EC\uC6B4\u2026"
title: "\uCF54\uB4DC\uB97C \uD568\uC218\uB85C \uAD6C\uC131\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇이며 왜 필요할까?
코드를 함수로 구성한다는 것은 코드를 특정 작업을 수행하는 재사용 가능한 덩어리로 나누는 것을 의미합니다. 프로그래머들은 코드를 읽고, 디버깅하며, 재사용하기 쉽게 만들기 위해서 이렇게 합니다. 레고를 상자에 분류하는 것과 같아서, 무언가를 만들고 싶을 때마다 혼돈스러운 더미를 뒤지지 않게 해줍니다.

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
