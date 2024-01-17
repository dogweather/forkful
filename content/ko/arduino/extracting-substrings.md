---
title:                "부분 문자열 추출하기"
html_title:           "Arduino: 부분 문자열 추출하기"
simple_title:         "부분 문자열 추출하기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

# 번역: Arduino 프로그래밍 기사

## 무엇 & 왜?
문자열 추출이란 무엇인지, 그리고 프로그래머들이 왜 이런 작업을 하는지에 대해서 간단히 설명합니다.

문자열 추출은 텍스트에서 원하는 부분을 추출하는 작업을 말합니다. 이를 통해 프로그래머는 필요한 정보만을 빠르게 얻을 수 있기 때문에 유용합니다.

## 하는 방법:
아두이노에서 문자열 추출을 하는 예제와 출력 결과를 ```Arduino ...``` 코드 블록 안에 넣어 설명합니다.

```
// 코드 예제
String text = "Hello World";
String subtext = text.substring(0, 5);
// Hello를 출력
Serial.println(subtext);
```

## 깊이 파고들기:
문자열 추출에 대한 역사적 배경, 대안들 및 구현 세부 사항과 같은 더 자세한 정보를 제공합니다.

문자열 추출은 프로그래밍 언어의 기본 기능으로 매우 중요하며, 대부분의 프로그래밍 언어에서 지원하고 있습니다. 따라서 여러 언어를 배울 필요 없이 다양한 프로그래밍 작업에서 유용하게 사용할 수 있습니다.

## 관련 자료:
문자열 추출에 대한 관련 자료를 제공합니다.

- 아두이노 공식 문서: https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/substring/
- 해외 개발자 블로그: https://www.codementor.io/@garethdwyer/working-with-strings-in-arduino-cjo81fw1j