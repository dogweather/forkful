---
title:                "Arduino: 문자열을 소문자로 변환하기"
simple_title:         "문자열을 소문자로 변환하기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 왜

문자열을 소문자로 변환하는 것이 왜 중요한지 궁금하신가요? 이 블로그 포스트에서 알려드리겠습니다!

## 방법

우선 ```Arduino```와 같은 프로그래밍 언어를 사용하여 문자열을 소문자로 변환하는 방법을 소개해 드리겠습니다. 아래 예제 코드를 참고해주세요.

```
Arduino String str = "HELLO";
str.toLowerCase();
Serial.println(str);
```

위 코드를 실행하면 ```hello```가 출력됩니다. 여기서 주의해야 할 점은 ```toLowerCase()``` 함수를 사용할 때 기존의 문자열이 아닌 소문자로 변환된 새로운 문자열이 반환된다는 것입니다. 따라서, 결과를 받을 변수를 따로 지정해주는 것이 좋습니다.

## 깊이 파고들기

문자열을 소문자로 변환하는 방법을 자세히 살펴보겠습니다. 문자열을 변환하는 가장 일반적인 방법은 각 문자를 확인하고 ASCII 코드를 사용하여 소문자로 변환하는 것입니다. 이는 배열과 반복문을 사용하여 구현할 수 있습니다. 하지만, 이보다 더 효율적인 방법으로 ```toLowerCase()``` 함수가 제공됩니다. 이 함수는 미리 정의된 테이블에 저장된 문자를 참조하여 소문자로 변환합니다. 따라서, 반복문을 사용하는 것보다 더 빠르게 문자열을 변환할 수 있습니다.

## 같이 보기

이 블로그 포스트로 문자열을 소문자로 변환하는 방법을 배우셨습니다. 하지만, 이 외에도 Arduino에서 유용한 함수들이 많이 존재합니다. 아래 링크들을 통해 관련 내용을 더 자세히 공부해보세요!

- [Arduino 공식 사이트](https://www.arduino.cc/)
- [String 함수 목록](https://www.arduino.cc/en/Reference/StringObject)
- [ASCII 코드표](http://www.asciitable.com/)