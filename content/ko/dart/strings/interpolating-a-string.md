---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:57.863664-07:00
description: "\uBB38\uC790\uC5F4 \uBCF4\uAC04\uC740 \uBCC0\uC218 \uAC12\uB4E4\uC744\
  \ \uBB38\uC790\uC5F4 \uC548\uC5D0 \uC9C1\uC811 \uC0BD\uC785\uD558\uB294 \uACFC\uC815\
  \uC73C\uB85C, \uBC88\uAC70\uB85C\uC6B4 \uC5F0\uACB0 \uC5C6\uC774 \uC758\uBBF8 \uC788\
  \uB294 \uBA54\uC2DC\uC9C0\uB97C \uC0DD\uC131\uD558\uAE30 \uC704\uD574 \uC790\uC8FC\
  \ \uC0AC\uC6A9\uB429\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uB354\
  \ \uAE54\uB054\uD558\uACE0 \uC77D\uAE30 \uC26C\uC6B4 \uCF54\uB4DC\uB97C \uC704\uD574\
  , \uADF8\uB9AC\uACE0 \uBCF5\uC7A1\uD55C \uBB38\uC790\uC5F4 \uC5F0\uACB0\uC5D0\uC11C\
  \ \uBC1C\uC0DD\uD558\uAE30 \uC26C\uC6B4 \uC624\uB958\uB4E4\uC744 \uBC29\uC9C0\uD558\
  \uAE30 \uC704\uD574 \uC774\uB97C \uC2E4\uD589\uD569\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:54.763913-06:00'
model: gpt-4-0125-preview
summary: "\uBB38\uC790\uC5F4 \uBCF4\uAC04\uC740 \uBCC0\uC218 \uAC12\uB4E4\uC744 \uBB38\
  \uC790\uC5F4 \uC548\uC5D0 \uC9C1\uC811 \uC0BD\uC785\uD558\uB294 \uACFC\uC815\uC73C\
  \uB85C, \uBC88\uAC70\uB85C\uC6B4 \uC5F0\uACB0 \uC5C6\uC774 \uC758\uBBF8 \uC788\uB294\
  \ \uBA54\uC2DC\uC9C0\uB97C \uC0DD\uC131\uD558\uAE30 \uC704\uD574 \uC790\uC8FC \uC0AC\
  \uC6A9\uB429\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uBCF4\uAC04\uD558\uAE30"
weight: 8
---

## 무엇 & 왜?

문자열 보간은 변수 값들을 문자열 안에 직접 삽입하는 과정으로, 번거로운 연결 없이 의미 있는 메시지를 생성하기 위해 자주 사용됩니다. 프로그래머들은 더 깔끔하고 읽기 쉬운 코드를 위해, 그리고 복잡한 문자열 연결에서 발생하기 쉬운 오류들을 방지하기 위해 이를 실행합니다.

## 방법:

Dart에서 문자열 보간은 `$` 기호를 사용하여 문자열 리터럴 내에서 직접 표현식을 보간하는 것이 간단합니다:

```dart
void main() {
  String name = 'Dart';
  int year = 2023;
  // 간단한 변수 보간
  print('Learning $name in $year!');
  // 출력: Learning Dart in 2023!
  
  // 표현식 보간
  print('In two years, it will be ${year + 2}.');
  // 출력: In two years, it will be 2025.
}
```

더 복잡한 표현식이 있거나 문자열 자체 내에서 연산을 수행하고 싶은 경우, 표현식을 `${}`로 감싸세요. Dart는 다양하고 복잡한 시나리오를 원활하게 처리할 수 있도록 기본적으로 잘 갖추어져 있어, 문자열 보간을 위한 특별히 인기 있는 서드파티 라이브러리를 가지고 있지 않습니다.
