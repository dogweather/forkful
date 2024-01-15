---
title:                "문자열의 길이 찾기"
html_title:           "Arduino: 문자열의 길이 찾기"
simple_title:         "문자열의 길이 찾기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 왜

문자열의 길이를 찾는 것은 많은 프로그래밍 작업에서 필수적입니다. 예를 들어, 문자열을 다루는 게임을 만들거나, 데이터를 처리하는 프로그램을 작성할 때 문자열의 길이를 알아야 합니다.

## 사용 방법

Arduino에서 문자열의 길이를 찾는 방법은 간단합니다. ```strlen()``` 함수를 사용하면 됩니다. 이 함수는 문자열의 길이를 반환합니다. 예를 들어, "Hello World"라는 문자열의 길이는 11이므로 ```strlen("Hello World")```는 11을 반환합니다.

```
Arduino
// Include the String library
#include <String.h>

void setup() {
    // Initialize serial communication
    Serial.begin(9600);

    // Define a string
    String str = "Hello World";

    // Find the length of the string
    int length = strlen(str);

    // Print the length
    Serial.println(length);
}

void loop() {
    // Empty loop
}
```

위의 코드를 실행하면 시리얼 모니터에 11이 출력되는 것을 볼 수 있습니다.

## 깊이 파헤치기

Arduino의 ```strlen()``` 함수는 문자열의 길이를 찾는 가장 간단하고 효율적인 방법입니다. 하지만 이 함수는 Null 문자를 발견하기 전까지 문자열의 길이를 계산하기 때문에 다른 문자가 추가되었거나 삭제되었을 때 불필요하게 길이가 달라질 수 있습니다. 따라서 정확한 결과를 얻기 위해서는 문자열에 Null 문자를 추가해주는 것이 좋습니다.

## 참고 자료

- [Arduino String Library Reference](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [Arduino strlen() 함수 문서](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/strlen/)