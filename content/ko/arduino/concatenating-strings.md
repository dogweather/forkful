---
title:                "문자열 연결하기"
html_title:           "Arduino: 문자열 연결하기"
simple_title:         "문자열 연결하기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why?
문자열 결합이란 무엇이고, 프로그래머들이 왜 이것을 하는지에 대해 간략히 설명합니다.

문자열 결합은 두 개 이상의 문자열을 하나의 문자열로 결합하는 것을 말합니다. 이는 한 문자열에 여러 가지 정보를 함께 표현할 수 있어 프로그래밍에서 자주 사용됩니다.

## How to:
아래의 코드 블록을 참고하여 코드 예제와 결과를 확인할 수 있습니다.

```Arduino
// 문자열 "Hello"와 "world"를 결합하는 예제
String first = "Hello";
String second = "world";
String result = first + second;
Serial.println(result);
```
결과: ```Helloworld```

```Arduino
// 숫자와 문자열을 결합하는 예제
int number = 10;
String word = "apples";
String result = String(number) + " " + word;
Serial.println(result);
```
결과: ```10 apples```

## Deep Dive:
(1) 문자열 결합은 1960년대 중반에 탄생한 언어인 ALGOL에서 처음 사용되었습니다.

(2) 문자열 결합의 대안으로는 문자열 포맷팅이 있습니다. 문자열 포맷팅은 문자열 내에 변수나 값을 삽입하여 보다 복잡한 문자열을 생성할 수 있습니다.

(3) 문자열 결합 기능은 아두이노에서 제공하는 String 라이브러리를 통해 사용할 수 있습니다. 이 라이브러리는 매우 쉬운 문법을 가지고 있어 사용자 편의성이 높습니다.

## See Also:
문자열 포맷팅: https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/formatstring/