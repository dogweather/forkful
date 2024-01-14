---
title:                "Arduino: 문자열의 길이 찾기"
simple_title:         "문자열의 길이 찾기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# 왜

문자열의 길이를 찾는 것은 Arduino 프로그래밍에서 중요한 부분입니다. 여러분이 만드는 프로그램에서 문자열을 다루는 경우, 그 길이를 알아야만 올바른 작업을 할 수 있습니다. 예를 들어, 사용자로부터 입력받은 문자열을 처리하거나, WiFi 모듈을 이용해 문자열을 전송할 때, 그 문자열의 길이를 알면 더 정확하고 효율적인 코드를 작성할 수 있습니다.

# 어떻게

문자열의 길이를 얻기 위해서는 `strlen()` 함수를 사용합니다. 이 함수는 `string.h` 라이브러리에 정의되어 있으며, 문자열의 길이를 반환합니다. 아래는 `strlen()` 함수를 사용한 예시 코드입니다.

```Arduino
#include <string.h>

void setup() {
  Serial.begin(9600); // 시리얼 통신 시작
  char myString[] = "HelloWorld"; // 처리할 문자열
  int length = strlen(myString); // 문자열의 길이를 받아서 length 변수에 저장
  Serial.print("The length of the string is: ");
  Serial.println(length); // 시리얼 모니터에 문자열의 길이 출력
}

void loop() {

}
```

위 코드를 실행하면, 시리얼 모니터에 "The length of the string is: 10" 이 출력될 것입니다. 이는 "HelloWorld" 문자열의 길이가 10이기 때문입니다.

# 깊이 파헤치기

C 언어에서는 문자열을 배열로 다루어야 합니다. 즉, 문자열은 문자의 배열로 이루어져 있고, 배열 마지막에는 NULL 문자 `\0` 가 있어야 합니다. 따라서 `strlen()` 함수는 문자열의 길이를 측정하기 위해 NULL 문자를 찾아서 그 위치를 반환합니다. 만약 NULL 문자가 없는 상황에서 `strlen()` 함수를 사용하면, 예상치 못한 결과를 얻게 될 수 있습니다.

또 다른 주의할 점은 UTF-8 인코딩을 사용하는 다국어 문자열의 경우, `strlen()` 함수가 정확한 길이를 반환하지 않을 수 있습니다. 이 경우, `String` 객체를 사용해 문자열의 길이를 측정하면 더 정확한 결과를 얻을 수 있습니다.

# 참고 자료

- [Arduino Reference - strlen()](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/strlen/)
- [Learn C.org - Strings](https://www.learn-c.org/en/Strings)
- [TechOnTheNet - C String Length](https://www.techonthenet.com/c_language/standard_library_functions/string_h/strlen.php)