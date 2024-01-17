---
title:                "정규식 사용하기"
html_title:           "Arduino: 정규식 사용하기"
simple_title:         "정규식 사용하기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 뭐지 & 왜쓰는거야?

🤔 정규표현식(regular expressions)이 뭔지 궁금해요? 간단히 말하면, 문자열을 찾아내는 패턴을 지정하는 표현식입니다. 프로그래머들은 이를 사용하여 특정 문자열을 추출하거나 패턴을 찾는 등의 작업을 할 수 있어요. 더 정확하고 효율적인 검색을 위해 정규표현식을 사용하는 것이죠.

## 어떻게 하나요?

아두이노에서 정규표현식을 사용하려면 우선 <regex.h> 라이브러리를 추가해야해요. 그 다음, ```Regex()``` 함수를 사용하여 입력된 문자열에 대해 패턴을 지정할 수 있어요. 적용된 패턴에 따라 함수가 true 또는 false를 반환하므로 if 문으로 조건을 설정할 수 있습니다. 아래는 간단한 예시입니다.

```
#include <regex.h>

regex pattern("<[a-zA-Z]+>");

void setup() {
  Serial.begin(9600);
  String sentence = "Hello, I am an Arduino board.";
  if (Regex(sentence)){
    Serial.println("This sentence contains HTML tag!");
  } else {
    Serial.println("No HTML tag in this sentence.");
  }
}

void loop() {
  // not used
}
```

위 코드를 실행하면 "This sentence contains HTML tag!"가 시리얼 모니터에 출력될 것입니다. 물론 간단한 예시지만, 이렇게 정규표현식을 사용하여 원하는 문자열을 추출하거나 조건에 따라 작업을 수행할 수 있습니다.

## 깊이 파헤치기

📚 정규표현식은 일반적으로 프로그래밍 언어의 표준 스펙이 아니지만 대부분의 언어에서 사용할 수 있습니다. 거의 모든 언어에서 라이브러리를 추가하거나 내장 함수를 사용하여 정규표현식을 쉽게 적용할 수 있습니다. 하지만 모든 문자열이 입력값으로 들어올 수 있고, 다양한 패턴을 지정해야 할 수 있으므로 정확한 패턴 작성이 필요합니다.

## 관련 자료

💻 정규표현식에 대해 더 알아보려면 다음 자료들을 참고해 보세요! 

- [Arduino 정규표현식 문서](https://www.arduino.cc/reference/ko/language/functions/communication/serial/if)
- [정규표현식 가이드 - w3schools](https://www.w3schools.com/jsref/jsref_obj_regexp.asp)
- [꼼곰한 정규표현식 튜토리얼 - Tutsplus](https://code.tutsplus.com/ko/tutorials/8-regular-expressions-you-should-know--net-6149)