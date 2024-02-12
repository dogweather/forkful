---
title:                "문자열을 소문자로 변환하기"
aliases:
- /ko/arduino/converting-a-string-to-lower-case/
date:                  2024-01-20T17:37:43.499141-07:00
model:                 gpt-4-1106-preview
simple_title:         "문자열을 소문자로 변환하기"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
문자열을 소문자로 변환한다는 것은, 모든 대문자 알파벳을 소문자로 바꾸는 것을 말합니다. 이는 데이터 처리나 사용자 입력을 표준화하기 위해 필요합니다.

## How to: (방법)
```Arduino
String originalString = "Hello, World!";
String lowerCaseString = originalString.toLowerCase();

void setup() {
  Serial.begin(9600);
  Serial.println(originalString); // "Hello, World!"
  Serial.println(lowerCaseString); // "hello, world!"
}

void loop() {
  // Nothing to do here
}
```
샘플 코드를 실행하면, 시리얼 모니터에 `Hello, World!`와 `hello, world!`가 출력됩니다.

## Deep Dive (심층 분석)
과거에는 문자열을 소문자로 변환하기 위해 각 문자를 직접 검사하고 ASCII 값으로 변환하는 과정이 필요했습니다. 하지만, 아두이노에서는 `String` 클래스의 `toLowerCase()` 함수가 이 작업을 간단히 처리해 줍니다. 대체 방법으로 C 스타일의 문자 배열을 사용하고, `tolower()` 함수를 각 문자에 적용할 수도 있습니다. 내부적으로, `toLowerCase()` 함수는 문자열에 포함된 각 문자를 ASCII 코드를 기준으로 소문자로 전환합니다.

## See Also (참고 자료)
- 아두이노 공식 문서 String `toLowerCase()` 함수: https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/tolowercase/
- C++ `tolower()` 함수에 대한 참고 자료: http://www.cplusplus.com/reference/cctype/tolower/
