---
title:                "Arduino: 문자열 대문자 변환하기"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 왜

문자열을 대문자로 바꾸는 것의 장점은 다양합니다. 예를 들어, 사용자의 입력을 처리하거나 데이터를 분석할 때 대문자와 소문자를 구분하는 것이 중요한 경우가 있습니다. 따라서 문자열을 대문자로 변환하여 일관성있게 처리할 수 있습니다.

## 어떻게

아래 예제 코드를 사용하여 문자열을 대문자로 바꾸는 방법을 살펴보겠습니다.

```Arduino 
// 사용자로부터 입력받은 문자열
String inputString = "Hello World!";

// 문자열을 대문자로 변환
String capitalizedString = inputString.toUpperCase();

// 변환된 문자열 출력
Serial.println(capitalizedString);
```

위 코드를 실행하면 콘솔에 "HELLO WORLD!"라는 결과가 출력됩니다. 이와 같이 `.toUpperCase()`를 사용하면 문자열을 대문자로 변환할 수 있습니다.

## 심층 분석

Arduino에서 `.toUpperCase()` 메소드는 `String` 클래스의 일부로 구현됩니다. 이 메소드는 주어진 문자열의 각 문자를 ASCII 코드를 이용하여 대문자로 변환합니다. 따라서 이 메소드를 사용하여 대소문자를 구분하지 않는 문자열 처리를 할 수 있습니다.

하지만 주의할 점은 이 메소드는 주어진 문자열을 수정하지 않고 새로운 문자열을 반환한다는 것입니다. 따라서 반환된 값을 새로운 변수에 저장하거나 출력해야 합니다.

## 참고자료

- [Arduino String 클래스 공식 문서](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/touppercase/)
- [ASCII 코드 표](https://www.asciitable.com/)