---
title:                "패턴에 일치하는 문자 삭제"
html_title:           "Fish Shell: 패턴에 일치하는 문자 삭제"
simple_title:         "패턴에 일치하는 문자 삭제"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 무엇이며, 왜 그래야 하는가?
문자열에서 일치하는 패턴의 문자를 삭제하는 것은 특정 문자 또는 문자 조합을 제거하는 프로그래밍 작업입니다. 이는 코드를 정리하거나 입력 오류를 방지하기 위해 매우 유용하게 사용됩니다.

## 어떻게 할 것인가:
아래의 간단한 코드 예제를 보겠습니다. 이 예제에서는 'Arduino'에서 모든 'r' 문자를 제거합니다.
```Arduino
String str = "Arduino";
str.replace("r", "");
Serial.println(str);
```
출력 결과는 아래와 같습니다:
```
Adino
```

## 깊이 들어가 보기:
이 작업은 프로그래밍의 초창기부터 사용되어 왔으며 현재 Arduino에서는 replace() 함수를 통해 이를 수행합니다. 같은 작업을 수행하는 다른 방법으로는 반복문을 통해 문자열을 스캔하고 일치하는 문자를 제거하는 방법이 있습니다. 하지만 replace() 함수가 이 작업에 가장 적합하며 효율적입니다. 

이 함수는 문자열 내의 모든 경우의 일치 문자를 찾아 대체 문자열로 교체합니다. 노트: 대체 문자열이 공백("")인 경우, 일치하는 문자는 제거됩니다.

## 참고 자료:
아래 링크에서 문자열 관련하여 더 많은 정보를 얻을 수 있습니다:

- Arduino 공식 문서: [String Functions](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/)
- Stack Overflow: [How to remove a character from a string using Arduino](https://stackoverflow.com/questions/14343812/how-to-remove-a-character-from-a-string-using-arduino)