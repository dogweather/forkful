---
title:                "텍스트 검색 및 교체"
aliases:
- /ko/arduino/searching-and-replacing-text/
date:                  2024-01-20T17:57:09.685527-07:00
model:                 gpt-4-1106-preview
simple_title:         "텍스트 검색 및 교체"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why? (무엇인가? 그리고 왜?)
텍스트 검색 및 교체는 특정 문자열을 찾아 다른 문자열로 바꾸는 과정입니다. 프로그래머들은 데이터 변경, 오류 수정, 코드 업데이트 등을 위해 이 작업을 수행합니다.

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
