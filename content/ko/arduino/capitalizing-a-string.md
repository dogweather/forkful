---
title:                "Arduino: 문자열 대문자 변환"
simple_title:         "문자열 대문자 변환"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 왜

문자열을 대문자로 변환하는 것이 중요한 이유는 실무에서 자주 사용되는 프로그래밍 요구 사항 중 하나입니다. 예를 들어, 사용자가 입력한 문자열을 모두 대문자로 변환하여 데이터 처리를 더 쉽게 할 수 있으며, 데이터베이스에서 검색 또는 비교를 할 때 대소문자를 구분하지 않기 위해서도 사용됩니다.

## 하는 방법

문자열을 대문자로 변환하는 방법에는 여러 가지가 있지만, 여기서는 Arduino에서 제공하는 `toupper()` 함수를 사용하는 방법을 알아보겠습니다. `toupper()` 함수는 문자 하나를 파라미터로 받아 해당 문자의 대문자를 반환하는 함수입니다. 따라서 이를 반복문을 통해 사용자가 입력한 문자열의 각 문자에 적용하여 대문자로 변환할 수 있습니다.

```Arduino
// 사용자가 입력한 문자열을 대문자로 변환하는 예제
String input = "hello world"; // 사용자가 입력한 문자열
String output = ""; // 변환된 문자열을 담을 변수

// 입력한 문자열을 한 글자씩 변환하여 output 변수에 추가
for (int i = 0; i < input.length(); i++) {
    output += toupper(input.charAt(i));
}

// 변환된 문자열 출력
Serial.println(output); // HELLO WORLD
``` 

위 예제에서는 `input` 변수에 사용자가 입력한 문자열을 저장하고, `output` 변수에는 변환된 문자열이 저장됩니다. `for` 반복문을 통해 `toupper()` 함수를 사용하여 각 문자를 대문자로 변환하고, `output` 변수에 추가하면서 저장합니다. 마지막으로 `output` 변수를 출력하여 변환된 문자열을 확인할 수 있습니다.

## 자세히 알아보기

Arduino에서 제공하는 `toupper()` 함수는 ASCII 코드 기준으로 문자를 대문자로 변환합니다. 따라서 영어 알파벳은 모두 대문자로 변환될 수 있지만, 다른 언어의 문자는 변환이 제대로 이루어지지 않을 수 있습니다. 또한, `toupper()` 함수는 문자가 소문자인 경우에만 대문자로 변환하며, 숫자나 특수 문자는 변환하지 않습니다.

만약 ASCII 코드 외의 다른 문자를 대문자로 변환하고 싶다면, `toupper()` 함수 대신 다른 유니코드 변환 함수를 사용해야 합니다. 또한, 대문자와 소문자를 완전히 구분하지 않아야 하는 경우에는 대문자로 변환한 후에도 비교를 위해 `tolower()` 함수를 사용해야 합니다.

## 참고

[ASCII 코드에 대한 자세한 설명 (Wikipedia)](https://ko.wikipedia.org/wiki/ASCII)

[유니코드 변환 함수 (Arduino 공식 문서)](https://www.arduino.cc/reference/en/language/functions/communication/touppercase/)

["toupper()" 함수에 대한 C++ 공식 문서](https://www.cplusplus.com/reference/cctype/toupper/)