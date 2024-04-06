---
date: 2024-01-20 17:57:09.685527-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) \uC544\uB450\uC774\uB178\
  \uC5D0\uC11C\uB294 \uBB38\uC790\uC5F4 \uB0B4\uC7A5 \uB77C\uC774\uBE0C\uB7EC\uB9AC\
  (String library)\uB85C \uD14D\uC2A4\uD2B8\uB97C \uAC80\uC0C9\uD558\uACE0 \uAD50\uCCB4\
  \uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4. `String.replace()` \uD568\uC218\uB97C \uC0AC\
  \uC6A9\uD558\uBA74 \uB429\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:57.235162-06:00'
model: gpt-4-1106-preview
summary: "(\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) \uC544\uB450\uC774\uB178\uC5D0\uC11C\
  \uB294 \uBB38\uC790\uC5F4 \uB0B4\uC7A5 \uB77C\uC774\uBE0C\uB7EC\uB9AC(String library)\uB85C\
  \ \uD14D\uC2A4\uD2B8\uB97C \uAC80\uC0C9\uD558\uACE0 \uAD50\uCCB4\uD560 \uC218 \uC788\
  \uC2B5\uB2C8\uB2E4."
title: "\uD14D\uC2A4\uD2B8 \uAC80\uC0C9 \uBC0F \uAD50\uCCB4"
weight: 10
---

## How to: (어떻게 하나요?)
아두이노에서는 문자열 내장 라이브러리(String library)로 텍스트를 검색하고 교체할 수 있습니다. `String.replace()` 함수를 사용하면 됩니다.

```Arduino
void setup() {
  // 시리얼 모니터 시작
  Serial.begin(9600);
  
  // 문자열 예제
  String text = "I love programming with Arduino.";
  
  // "love"를 "enjoy"로 교체
  text.replace("love", "enjoy");
  
  // 결과 출력
  Serial.println(text); // "I enjoy programming with Arduino."
}

void loop() {
  // Not used in this example.
}
```

## Deep Dive (깊이 알아보기)
텍스트 교체는 컴퓨터 프로그래밍 초기부터 있었습니다. ‘sed’ 같은 유닉스 명령어 도구를 사용해서 터미널에서 교체를 수행했었습니다. 아두이노에서는 `String` 클래스 내의 `replace()` 함수로 간단하게 교체할 수 있지만 메모리 관리가 중요합니다. 큰 데이터에서 문자열 처리 시, 동적할당으로 인한 프래그먼테이션 문제가 생길 수 있으니 주의가 필요합니다.

## See Also (함께 보기)
- [Arduino String Reference](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- [Arduino Memory](https://www.arduino.cc/en/Tutorial/Foundations/Memory)
- [Using Flash Memory with PROGMEM](https://www.arduino.cc/reference/en/language/variables/utilities/progmem/)
