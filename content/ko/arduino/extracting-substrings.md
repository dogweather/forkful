---
title:                "부분 문자열 추출"
html_title:           "Arduino: 부분 문자열 추출"
simple_title:         "부분 문자열 추출"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## 왜
Substring 추출을 할 때 중요한 것은 문자열의 일부분을 뽑아내는 것입니다. 이를 통해 우리는 원하는 데이터를 더 쉽게 다룰 수 있고, 필요한 정보만 선별해서 사용할 수 있습니다.

## 방법
Arduino에서 Substring 추출하는 방법은 간단합니다. 먼저 `substring()` 함수를 사용하여 추출할 부분의 인덱스 값을 지정한 다음, 해당 부분을 저장하고 원하는 작업을 수행하면 됩니다. 예제 코드와 출력 결과는 아래와 같습니다.

```Arduino
String str = "Greetings from Arduino";
String sub = str.substring(10, 16); // 10번째부터 16번째까지의 문자를 추출하여 `sub` 변수에 저장
Serial.println(sub); // "Arduino" 출력
```

## Deep Dive
Substring 추출은 문자열 처리에서 매우 중요한 역할을 합니다. `substring()` 함수는 인덱스 값을 기준으로 문자열을 추출하기 때문에, 정확한 인덱스 값을 지정하는 것이 중요합니다. 또한, `substring()` 함수는 원본 문자열을 변경하지 않기 때문에 추출한 문자열을 저장할 변수를 별도로 선언해야 합니다. 따라서, 다양한 문자열 처리를 할 때 유용하게 사용할 수 있습니다.

## 더보기
- [Arduino String 클래스 공식 문서](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- [Java substring() 메서드 공식 문서](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#substring(int,%20int))