---
date: 2024-01-20 17:47:04.049974-07:00
description: "How to: \uC544\uB450\uC774\uB178\uC5D0\uC11C \uBB38\uC790\uC5F4\uC758\
  \ \uAE38\uC774\uB97C \uAD6C\uD558\uB294 \uAC83\uC740 \uCF54\uB529\uC758 \uAE30\uBCF8\
  \ \uC911 \uD558\uB098\uC785\uB2C8\uB2E4. 'String' \uD074\uB798\uC2A4\uC758 'length()'\
  \ \uBA54\uC11C\uB4DC\uB97C \uC4F0\uBA74 \uC27D\uAC8C \uAE38\uC774\uB97C \uAD6C\uD560\
  \ \uC218 \uC788\uC8E0. \uC774 \uBC29\uBC95\uC740 \uCEF4\uD4E8\uD130 \uD504\uB85C\
  \uADF8\uB798\uBC0D \uC5B8\uC5B4\uC758 \uC2DC\uC791\uBD80\uD130 \uC788\uC5C8\uB358\
  \ \uAE30\uB2A5\uC785\uB2C8\uB2E4. \uB2E4\uB978 \uBC29\uBC95\uB3C4 \uC788\uC5B4\uC694\
  . \uC608\uB97C \uB4E4\uC5B4,\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:57.241592-06:00'
model: gpt-4-1106-preview
summary: "\uC544\uB450\uC774\uB178\uC5D0\uC11C \uBB38\uC790\uC5F4\uC758 \uAE38\uC774\
  \uB97C \uAD6C\uD558\uB294 \uAC83\uC740 \uCF54\uB529\uC758 \uAE30\uBCF8 \uC911 \uD558\
  \uB098\uC785\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4\uC758 \uAE38\uC774 \uCC3E\uAE30"
weight: 7
---

## How to:
```Arduino
void setup() {
    Serial.begin(9600);
    String greeting = "안녕하세요!";
    Serial.println(greeting.length()); // 문자열의 길이를 출력합니다.
}

void loop() {
    // 여기서는 아무것도 하지 않습니다.
}
```
샘플 출력:
```
6
```

## Deep Dive
아두이노에서 문자열의 길이를 구하는 것은 코딩의 기본 중 하나입니다. 'String' 클래스의 'length()' 메서드를 쓰면 쉽게 길이를 구할 수 있죠. 이 방법은 컴퓨터 프로그래밍 언어의 시작부터 있었던 기능입니다.

다른 방법도 있어요. 예를 들어, 'strlen()' 함수를 사용해 'char' 배열의 길이를 구할 수 있습니다. 그러나 이 함수는 '\0'(null 문자)가 있는 C 스타일의 문자열에서만 작동하니 주의가 필요합니다.

길이를 찾는 과정의 세부 구현은 아두이노 라이브러리 소스코드 안에서 확인할 수 있어요. 'String' 객체는 내부에서 문자 배열을 관리하며, 'length()' 메서드는 배열의 현재 크기를 바로 알려줍니다.

## See Also
- 아두이노 String 레퍼런스: [Arduino String Reference](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- 'strlen()' 함수와 관련된 정보: [C++ strlen function](http://www.cplusplus.com/reference/cstring/strlen/)
