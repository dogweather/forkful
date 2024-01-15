---
title:                "문자열을 소문자로 변환하기"
html_title:           "Arduino: 문자열을 소문자로 변환하기"
simple_title:         "문자열을 소문자로 변환하기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 왜

자바스크립트 실무자라면 모든 문자를 소문자로 변환하는 것이 중요할 수 있습니다. 이는 검색 또는 비교 기능을 구현할 때, 모두 소문자 형태의 문자열을 사용해야 한다는 알게 모를 때 문제가 될 수 있기 때문입니다.


## 방법

본격적으로 아두이노에서 문자열을 대문자 또는 소문자로 변환하는 방법을 살펴보겠습니다. 아래의 코드를 사용하면 쉽게 원하는 결과를 얻을 수 있습니다.

```Arduino
// 문자열을 소문자로 변환하는 코드
String inputString = "Hello World!";
String outputString = inputString.toLowerCase();

// 출력 결과
Serial.println(outputString);
// Output: hello world!
```

위의 코드에서 우리는 먼저 변환하고 싶은 문자열을 `inputString` 변수에 담았습니다. 그리고 `toLowerCase()` 메소드를 사용하여 `inputString` 문자열을 소문자로 변환한 후, `outputString` 변수에 저장했습니다. 마지막으로 `Serial.println()` 함수를 사용하여 결과를 출력했습니다.

## 깊이 파고들기

아두이노에서 문자열을 대문자 또는 소문자로 변환할 때 사용하는 메소드는 `toUpperCase()`와 `toLowerCase()` 뿐만 아니라 `capitalize()` 메소드도 있습니다. 이 메소드는 문자열에서 첫 번째 글자를 대문자로 변환해주는 역할을 합니다.

또한 대문자 또는 소문자로 변환된 문자열이 필요하지 않은 경우, `.toUpperCase()` 코딩을 사용하여 변환하는 대신 `if()` 문과 `ASCII` 코드를 함께 사용할 수도 있습니다. 이 방법을 사용하면 더 많은 유연성을 가지고 원하는 결과를 얻을 수 있습니다.

## 관련 정보

[Arduino String 클래스 참조](https://www.arduino.cc/en/Reference/String) - 아두이노 공식 사이트에서 String 클래스에 대한 자세한 설명을 확인할 수 있습니다.

[ASCII 표](https://www.asciitable.com/) - 문자를 ASCII 코드로 변환할 수 있는 표를 제공합니다.

[아두이노 공식 튜토리얼](https://www.arduino.cc/en/Tutorials) - 아두이노의 다양한 예제 코드와 튜토리얼을 제공합니다.