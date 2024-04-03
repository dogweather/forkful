---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:57.863664-07:00
description: "\uBC29\uBC95: Dart\uC5D0\uC11C \uBB38\uC790\uC5F4 \uBCF4\uAC04\uC740\
  \ `$` \uAE30\uD638\uB97C \uC0AC\uC6A9\uD558\uC5EC \uBB38\uC790\uC5F4 \uB9AC\uD130\
  \uB7F4 \uB0B4\uC5D0\uC11C \uC9C1\uC811 \uD45C\uD604\uC2DD\uC744 \uBCF4\uAC04\uD558\
  \uB294 \uAC83\uC774 \uAC04\uB2E8\uD569\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:54.763913-06:00'
model: gpt-4-0125-preview
summary: "Dart\uC5D0\uC11C \uBB38\uC790\uC5F4 \uBCF4\uAC04\uC740 `$` \uAE30\uD638\uB97C\
  \ \uC0AC\uC6A9\uD558\uC5EC \uBB38\uC790\uC5F4 \uB9AC\uD130\uB7F4 \uB0B4\uC5D0\uC11C\
  \ \uC9C1\uC811 \uD45C\uD604\uC2DD\uC744 \uBCF4\uAC04\uD558\uB294 \uAC83\uC774 \uAC04\
  \uB2E8\uD569\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uBCF4\uAC04\uD558\uAE30"
weight: 8
---

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
