---
title:                "문자열 대문자로 변환하기"
date:                  2024-01-19
html_title:           "Arduino: 문자열 대문자로 변환하기"
simple_title:         "문자열 대문자로 변환하기"

category:             "Arduino"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
문자열 대문자 변환은 모든 문자를 대문자로 바꾸는 것입니다. 가독성을 높이거나 일관된 데이터 형식이 필요할 때 사용합니다.

## How to: (어떻게 하나요?)
```Arduino
void setup() {
  Serial.begin(9600);
  String message = "Arduino is awesome!";
  message.toUpperCase();
  Serial.println(message);  // ARDUINO IS AWESOME!
}

void loop() {
  // Nothing here
}
```

## Deep Dive (깊이 알아보기)
문자열을 대문자로 변환하는 것은 많은 프로그래밍 언어에 공통적인 기능입니다. Arduino에서 `toUpperCase()` 함수는 문자열 내의 모든 소문자를 대문자로 변경합니다. C나 C++와 같은 Arduino의 기반 언어에서는 대문자 변환을 위해 `toupper()` 함수를 각 문자에 적용해야 합니다. 하지만 Arduino는 이를 간단하게 한 번의 호출로 처리할 수 있는 `String` 클래스를 제공합니다. 또한, 성능이 중요한 상황에서는 `String` 대신 C 스타일의 문자 배열을 사용하는 것이 더 효율적일 수 있습니다.

## See Also (관련 자료)
- [Arduino Reference: toUpperCase()](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/touppercase/)
- [Arduino String Object](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- [C++ toupper()](https://www.cplusplus.com/reference/cctype/toupper/)
