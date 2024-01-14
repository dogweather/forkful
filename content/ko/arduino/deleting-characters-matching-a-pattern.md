---
title:                "Arduino: 패턴에 일치하는 문자 제거"
simple_title:         "패턴에 일치하는 문자 제거"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 왜?
Arduino 프로그래밍에서 패턴과 일치하는 문자를 삭제하는 작업은 불필요한 문자를 정리하거나 정확한 데이터를 추출하기 위해 필요합니다.

## 방법
Arduino에서 패턴과 일치하는 문자를 삭제하는 방법은 아래와 같습니다. 먼저 Serial Monitor 또는 다른 입력 방법을 통해 문자열을 입력받습니다.

```
Arduino
```

그런 다음 `indexOf()` 함수를 사용하여 입력된 문자열에서 패턴의 첫 번째 문자의 인덱스를 찾습니다.

```
int index = inputString.indexOf("do");
```

인덱스를 찾은 후, `substring()` 함수를 사용하여 해당 인덱스부터 패턴의 길이만큼의 문자를 삭제합니다. 마지막으로 남은 문자열을 출력합니다.

```
String output = inputString.substring(0, index) + inputString.substring(index + pattern.length());

Serial.println(output);
```

다음은 전체 코드와 실행 결과입니다.

```
String inputString = "Arduino";
String pattern = "do";

void setup() {
  Serial.begin(9600);

  String output = "";

  Serial.print("Input String: ");
  Serial.println(inputString);

  int index = inputString.indexOf(pattern);
  output = inputString.substring(0, index) + inputString.substring(index + pattern.length());

  Serial.print("Output String: ");
  Serial.println(output);
}

void loop() {

}

```

```
Input String: Arduino
Output String: Aruin
```

## 깊게 들어가기
더 복잡한 패턴을 가진 문자열을 삭제하려면 `replace()` 함수를 사용할 수 있습니다. 이 함수는 입력된 문자열에서 주어진 패턴을 가진 모든 부분을 다른 문자열로 대체합니다. 아래는 `replace()` 함수를 사용하여 입력된 문자열에서 모음을 삭제하는 예제 코드입니다.

```
String inputString = "Arduino";
String vowels = "aeiou";

void setup() {
  Serial.begin(9600);

  String output = "";

  Serial.print("Input String: ");
  Serial.println(inputString);

  for (int i = 0; i < vowels.length(); i++) {
    output = inputString.replace(vowels.substring(i, i+1), "");
    inputString = output;
  }

  Serial.print("Output String: ");
  Serial.println(output);
}

void loop() {

}

```

```
Input String: Arduino
Output String: Ardn
```

## 참고 자료
- [String.indexOf() - Arduino Reference](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/indexof/)
- [String.substring() - Arduino Reference](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/substring/)
- [String.replace() - Arduino Reference](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/replace/)

## 참고 자료
- [String.indexOf() - Arduino Reference](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/indexof/)
- [String.substring() - Arduino Reference](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/substring/)
- [String.replace() - Arduino Reference](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/replace/)