---
title:                "문자열의 길이 찾기"
html_title:           "Arduino: 문자열의 길이 찾기"
simple_title:         "문자열의 길이 찾기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# 무엇이며 왜 거기에 대해?

String의 길이를 찾는 것은 프로그래머들이 자주 하는 작업입니다. String은 문자들의 시퀀스이며, 우리는 때로는 그 길이를 알아야 할 때가 있습니다. 예를 들어, 사용자로부터 입력받은 문자열이 얼마나 긴지 알고 싶을 수도 있습니다. 또한 문자열 길이는 메모리 할당과 관련하여 중요한 요소입니다.

# 어떻게 하나요?

코드 블록( ```Arduino ...``` ) 내에서 예제 코드와 샘플 출력을 확인할 수 있습니다.

### 예시 1: String의 길이 구하기
```
String myString = "Hello World";
int length = myString.length();
Serial.println(length); // 11 출력
```

### 예시 2: 사용자로부터 입력받은 문자열의 길이 구하기
사용자로부터 입력받은 문자열을 ```serial.readString()``` 함수를 이용해서 읽어온 후, 그 길이를 구할 수 있습니다.

```
Serial.println("Input a string:");
String input = Serial.readString();
int length = input.length();
Serial.println(length); // 입력받은 문자열의 길이 출력
```

# 더 탐구해보기

## 역사적인 배경

String의 길이를 알아내는 작업은 컴퓨터 과학의 초기부터 존재했습니다. C언어에서는 ```strlen()``` 함수를 이용해 문자열의 길이를 구할 수 있었고, 현재 대부분의 프로그래밍 언어에서도 비슷한 기능을 제공합니다.

## 다른 방법들

문자열의 길이를 구하기 위해 다양한 알고리즘이 개발되어 왔습니다. 일반적으로 가장 짧은 시간에 실행될 수 있는 알고리즘을 선택합니다. 하지만 에너지 효율 등의 다른 요소를 고려할 때, 최적의 알고리즘을 선택하는 것은 어려운 문제입니다.

## 구현 세부사항

문자열의 길이를 구하기 위해서는 문자열의 마지막에 null 문자가 있어야만 합니다. null 문자는 문자열의 끝을 나타내는 특수한 문자로, C언어에서는 ```\0``` 로 표현됩니다.

# 관련 자료

- [Arduino String 라이브러리 문서](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- [WikiBooks: C Programming - Null Terminated Strings](https://en.wikibooks.org/wiki/C_Programming/Null_terminated_strings)
- [Stack Overflow: Efficiently finding a string length: longest common substring](https://stackoverflow.com/questions/11173370/efficiently-finding-a-string-length-longest-common-substring)