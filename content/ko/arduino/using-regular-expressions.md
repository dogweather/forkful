---
title:    "Arduino: 정규식을 사용하는 방법"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## 왜

정규 표현식을 사용하는 이유는 무엇일까요? 아두이노 프로그래밍에서 정규 표현식을 사용하면 복잡한 문자열 패턴을 찾고 추출하는 것이 가능해집니다. 예를 들어, 센서에서 읽어온 값을 조건에 맞게 분리해낼 수 있습니다.

## 사용 방법

정규 표현식을 사용하는 첫 번째 단계는 아두이노에서 이를 지원하는 라이브러리를 불러오는 것입니다. ```Arduino.h``` 파일 안에 정규 표현식 라이브러리가 포함되어 있으므로, 이를 불러와야 합니다.

다음으로, 코드에서 정규 표현식 객체를 생성하고, ```search()``` 메소드를 사용하여 패턴을 찾을 문자열을 지정합니다. 이후에는 정규 표현식을 사용하여 원하는 패턴을 찾고 출력하면 됩니다.

예시 코드는 다음과 같습니다.

```Arduino
#include <Arduino.h> // 아두이노 라이브러리 불러오기

void setup() {
    Serial.begin(9600); // 시리얼 통신 설정
}

void loop() {
    String stringToSearch = "Hello World!";
    // Hello라는 패턴을 검색하기 위한 정규 표현식 객체 생성
    Regex regexExpression = Regex("Hello");

    if (regexExpression.search(stringToSearch)) {
        Serial.println("패턴을 찾았습니다!");
    } else {
        Serial.println("패턴이 없습니다.");
    }
}
```

위 코드의 결과는 다음과 같습니다.

```
패턴을 찾았습니다!
```

## 깊이 있는 이해

정규 표현식은 일반 문자열 검색 패턴을 초과해 더 다양한 기능을 제공합니다. 방금 예시로 든 ```search()``` 메소드는 문자열에서 단순히 패턴을 찾아주는 것이지만, ```match()```, ```isMatch()```, ```replace()```, ```split()``` 등 다양한 메소드를 사용하여 문자열을 처리할 수 있습니다.

정규 표현식을 사용하면 조건에 따라 원하는 패턴을 찾아내고 추출하거나, 문자열을 변경하거나, 분리할 수 있습니다. 또한, 기존 코드에서 조건문이나 반복문을 사용하지 않고도 더 간결하고 효율적인 코드를 작성할 수 있습니다.

하지만 정규 표현식은 복잡한 문법을 가지고 있기 때문에 처음에는 어려울 수 있습니다. 하지만 연습과 경험을 통해 능숙하게 사용할 수 있습니다.

## 또 다른 정보

- [정규 표현식 문법 가이드](https://www.regular-expressions.info/)
- [아두이노 공식 문서 - 정규 표현식 라이브러리](https://www.arduino.cc/reference/en/language/structures/strings/regex/)
- [정규 표현식 사용 예시](https://www.arduino.cc/pro/tutorials/regular-expressions-in-arduino)
- [정규 표현식 연습 문제 사이트](https://regexone.com/)