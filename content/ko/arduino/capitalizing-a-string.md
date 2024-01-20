---
title:                "문자열 대문자화"
html_title:           "Arduino: 문자열 대문자화"
simple_title:         "문자열 대문자화"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 무엇이고 왜 필요한가?

문자열 대문자화는 모든 알파벳 문자를 대문자로 변환하는 것을 말합니다. 사용자의 입력이 사용자의 의도와 일치하도록 하기 위해, 프로그래머들은 일반적으로 이를 사용합니다.

## 어떻게 하는가:

다음은 Arduino에서 문자열을 대문자로 변환하는 예입니다:

```Arduino
void setup() {
  Serial.begin(9600);
}

void loop() {
  String str = "hello world";
  str.toUpperCase();
  Serial.println(str);
  delay(1000);
}
```
이 코드를 실행하면, 시리얼 모니터에 "HELLO WORLD"가 매 초마다 출력됩니다.

## 깊게 알아보기

- **역사적 맥락**: 과거의 컴퓨터 시스템들은 대소문자 구분이 없었기 때문에, 문자열 대문자화는 필요했습니다. 오늘날에는 여전히 유용성을 유지하고 있습니다.
- **대안**: 대문자로 변환할 필요가 없는 경우, 문자열 비교를 위해 `equalsIgnoreCase()`와 같은 메서드 사용을 고려할 수 있습니다. 조그마한 메모리 절약 효과를 가져옵니다.
- **구현 세부정보**: Arduino에서, `toUpperCase()` 메서드는 문자열 안의 모든 문자를 ASCII 값에 따라 대문자로 변환하는 방법을 사용합니다.

## 참고 자료

- Arduino 공식 documentation의 [문자열 메서드](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/) 페이지에서 `toUpperCase()`에 대해 더 자세히 알아볼 수 있습니다.

- C++의 [문자열 관련 메서드](http://www.cplusplus.com/reference/string/string/) 및 Java의 [문자열 관련 메서드](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html)는 같은 도구를 제공합니다. 이들 언어의 `toUpperCase()` 메서드는 Arduino의 것과 개념적으로 비슷합니다.