---
title:                "Arduino: 문자열을 소문자로 변환하기"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 왜 

아두이노 프로그래밍에서 문자열을 소문자로 변환하는 것은 매우 유용합니다. 이것은 사용자로 하여금 입력된 문자열을 문자 크기를 구분하지 않고 처리할 수 있게 해주기 때문입니다.

## 어떻게

아두이노를 사용하여 문자열을 소문자로 변환하는 방법은 다음과 같습니다. 아래의 예시 코드를 참고하세요.

```Arduino
// 사용자로부터 문자열 입력 받기
String text = Serial.readString();

// 입력받은 문자열을 소문자로 변환
text.toLowerCase();

// 변환된 문자열을 시리얼 모니터에 출력
Serial.print(text);
```

위의 코드를 실행하면 아두이노가 사용자로부터 문자열을 입력받고, 입력받은 문자열을 소문자로 변환하여 다시 출력합니다. 예를 들어, "HELLO"라는 문자열을 입력하면 "hello"라는 결과가 나오게 됩니다.

## 심층 분석

문자열을 소문자로 변환하는 방법은 다양한 방식으로 가능합니다. 아래는 `toLowerCase()` 함수를 사용한 예시 코드입니다.

```Arduino
String text = "Hello";

for (int i = 0; i < text.length(); i++) {
    // 각 문자를 소문자로 변환
    char c = toLowerCase(text.charAt(i));
    // 변환된 문자를 시리얼 모니터에 출력
    Serial.print(c);
}
```

위의 코드는 `toLowerCase()` 함수를 사용하여 각 문자를 소문자로 변환하고, 변환된 문자를 시리얼 모니터에 출력합니다. 따라서 "Hello"라는 문자열을 입력하면 "hello"라는 결과가 나오게 됩니다.

## 참고 자료

- [String.toLowerCase() - Arduino Reference](https://www.arduino.cc/reference/ko/language/variables/data-types/string/functions/tolowercase/)
- [Arduino - String toLowerCase() - GeeksforGeeks](https://www.geeksforgeeks.org/arduino-string-tolowercase/)
- [String.charAt() - Arduino Reference](https://www.arduino.cc/reference/ko/language/variables/data-types/string/functions/charat/)