---
title:                "문자열 연결하기"
date:                  2024-01-20T17:34:15.810610-07:00
model:                 gpt-4-1106-preview
simple_title:         "문자열 연결하기"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
문자열 연결은 여러 문자열 조각들을 하나로 합치는 작업입니다. 데이터를 쉽게 조작하고 사용자에게 정보를 명확하게 보여주기 위해 프로그래머들이 사용합니다.

## How to: (방법)
```Arduino
void setup() {
  Serial.begin(9600);
  
  String part1 = "안녕, ";
  String part2 = "아두이노!";
  String combined = part1 + part2;

  Serial.println(combined);
}

void loop() {
  // Do nothing here
}
```
출력:
```
안녕, 아두이노!
```

## Deep Dive (심층 분석)
문자열 연결은 아두이노의 초기 버전부터 사용되어 왔지만, 동적 메모리 관리가 많이 개선되었습니다. 또한, `+` 연산자를 사용하는 것 외에도 `concat()` 메소드나 `sprintf()` 함수를 사용하여 문자열을 합칠 수도 있습니다. 그러나 `String` 객체를 사용하면 동적 메모리가 할당되기 때문에 메모리가 조각화될 위험이 있습니다. 이를 피하기 위해, 메모리 사용을 더 잘 제어할 수 있는 `strcat()` 함수를 사용한 문자 배열(char array)을 활용할 수도 있습니다.

## See Also (참고하기)
- [Arduino Reference: String Object](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- [Arduino Forum discussion on Strings](https://forum.arduino.cc/t/an-introduction-to-string-handling-in-arduino/583891)
- [Arduino Memory Management](https://www.arduino.cc/en/Tutorial/Foundations/Memory)
