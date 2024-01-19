---
title:                "텍스트 검색 및 교체"
html_title:           "Elixir: 텍스트 검색 및 교체"
simple_title:         "텍스트 검색 및 교체"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

텍스트 검색 및 대체는 문자열에서 특정 텍스트를 찾아 다른 텍스트로 변경하는 것입니다. 이는 데이터 정리 또는 수정이 필요한 경우 프로그래머가 사용하는 일반적인 기술입니다.

## 어떻게:

아래는 Arduino에서 텍스트를 검색하고 대체하는 방법을 보여주는 코드 예제입니다.

```Arduino
String text = "안녕, 세상!";
Serial.println(text);
text.replace("세상", "Arduino");
Serial.println(text);
```
이 코드는 "안녕, 세상!"이라는 텍스트를 출력한 다음, "세상"을 "Arduino"로 대체하여 "안녕, Arduino!"를 출력합니다.

## 심층 탐구:

단순히 Arduino 환경에서 제공하는 `replace` 함수를 사용하는 것이 가장 쉬운 방법이지만, 이를 수행하는 다른 방법도 많습니다. 또한 `replace` 함수는 먼저 알고리즘 방식으로 1974년에 등장한 Unix의 `sed` 스트림 편집기에서 비롯되었음을 알 수 있습니다. 이는 위치나 패턴을 찾아 문자열에서 교체하는 기능을 제공했습니다.

Arduino에서는 또한 `strstr` 라이브러리 함수를 사용하여 문자열에서 특정 패턴을 찾을 수도 있습니다. 그러나 이 경우 교체 함수를 직접 구현해야 할 수 있습니다. 이는 더 많은 코드와 처리 시간을 필요로 하지만, 필요한 경우 사용자에게 더 큰 유연성을 제공합니다.

## 참고 자료:

-Arduino String Reference: [`https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/`](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
  
-Understanding Arduino Programming: [`https://learn.sparkfun.com/tutorials/understanding-arduino-programming`](https://learn.sparkfun.com/tutorials/understanding-arduino-programming)

-Unix `sed` command: [`https://www.geeksforgeeks.org/sed-command-in-linux-unix-with-examples/`](https://www.geeksforgeeks.org/sed-command-in-linux-unix-with-examples/)