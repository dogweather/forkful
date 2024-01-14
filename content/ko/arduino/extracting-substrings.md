---
title:    "Arduino: 부분 문자열 추출"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## 왜
문자열에서 일부 문자열을 추출하는 것은 많은 유용한 기능 중 하나입니다. 예를 들어, 사용자가 입력한 전화번호를 특정 형식으로 변환하거나, 웹사이트에서 특정 키워드를 검색 할 수 있습니다. 아두이노에서도 문자열 추출 기능을 사용하면 다양한 프로젝트를 더욱 효율적이고 정확하게 구현할 수 있습니다.

## 추출하는 법
추출하고자 하는 문자열의 시작과 끝 인덱스를 지정하여 `substring()` 함수를 사용하면 됩니다. 아래의 예제는 문자열에서 3번째 문자부터 6번째 문자까지 추출하는 코드입니다.

```Arduino
String message = "안녕하세요!";
String extract = message.substring(3, 6);
Serial.println(extract);
```

출력 결과는 `녕하세`가 됩니다. 또한, `substring()` 함수를 이용하여 추출한 문자열을 다른 변수에 저장하여 더 다양한 활용이 가능합니다. 예를 들어, 추출한 부분 문자열을 `if`문의 조건으로 사용하여 원하는 동작을 수행할 수 있습니다.

## 깊은 곳으로
`substring()` 함수는 문자열 객체에만 사용할 수 있는 것이 아니라, 문자열 배열에서도 사용이 가능합니다. 또한, 시작 인덱스가 0이 아닌 다른 값을 지정하여도 문자열을 추출할 수 있습니다. 또한, `indexOf()` 함수를 사용하여 원하는 문자나 단어가 포함된 위치를 찾은 후, 해당 위치부터 추출을 할 수도 있습니다.

위의 예제에서는 시작 인덱스와 끝 인덱스의 값을 직접 지정하였지만, `substring()` 함수의 두 번째 매개변수에 음수 값을 지정하여 문자열의 뒤에서부터 추출하는 것도 가능합니다.

## 더 알아보기
아두이노에서 문자열을 다루는 다른 기능에 대해 더 알아보고 싶다면 아래의 링크들을 참고하시기 바랍니다.

- [Arduino Reference - String Substring()](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/substring/)
- [Arduino String 검색 문서](https://www.arduino.cc/reference/ko/language/variables/data-types/stringobject/)
- [프로젝트 아두이노 - 문자열 다루기](https://project.arduino.kr/chapter/learn/essential-arduino-sketch#define-substring)

## 이어서 보기