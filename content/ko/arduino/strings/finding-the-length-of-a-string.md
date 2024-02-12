---
title:                "문자열의 길이 찾기"
aliases:
- /ko/arduino/finding-the-length-of-a-string/
date:                  2024-01-20T17:47:04.049974-07:00
model:                 gpt-4-1106-preview
simple_title:         "문자열의 길이 찾기"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
문자열 길이 찾기는 무엇이고, 왜 프로그래머들이 이것을 할까?
문자열 길이 찾기란, 문자들의 숫자를 세는 과정입니다. 프로그래머들은 데이터 처리, 유효성 검사, 혹은 화면에 표시하기 전에 문자열의 크기를 알아야 할 때 이를 사용합니다.

## How to:
```Arduino
void setup() {
    Serial.begin(9600);
    String greeting = "안녕하세요!";
    Serial.println(greeting.length()); // 문자열의 길이를 출력합니다.
}

void loop() {
    // 여기서는 아무것도 하지 않습니다.
}
```
샘플 출력:
```
6
```

## Deep Dive
아두이노에서 문자열의 길이를 구하는 것은 코딩의 기본 중 하나입니다. 'String' 클래스의 'length()' 메서드를 쓰면 쉽게 길이를 구할 수 있죠. 이 방법은 컴퓨터 프로그래밍 언어의 시작부터 있었던 기능입니다.

다른 방법도 있어요. 예를 들어, 'strlen()' 함수를 사용해 'char' 배열의 길이를 구할 수 있습니다. 그러나 이 함수는 '\0'(null 문자)가 있는 C 스타일의 문자열에서만 작동하니 주의가 필요합니다.

길이를 찾는 과정의 세부 구현은 아두이노 라이브러리 소스코드 안에서 확인할 수 있어요. 'String' 객체는 내부에서 문자 배열을 관리하며, 'length()' 메서드는 배열의 현재 크기를 바로 알려줍니다.

## See Also
- 아두이노 String 레퍼런스: [Arduino String Reference](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- 'strlen()' 함수와 관련된 정보: [C++ strlen function](http://www.cplusplus.com/reference/cstring/strlen/)
