---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:55.499953-07:00
description: "\uC5B4\uB5BB\uAC8C: Dart\uC5D0\uC11C\uB294 `substring()`, `split()`,\
  \ \uC815\uADDC \uD45C\uD604\uC2DD\uACFC \uAC19\uC740 \uB2E4\uC591\uD55C \uBA54\uC11C\
  \uB4DC\uB97C \uC0AC\uC6A9\uD558\uC5EC \uBD80\uBD84 \uBB38\uC790\uC5F4\uC744 \uCD94\
  \uCD9C\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4. \uAC01 \uBA54\uC11C\uB4DC\uB294 \uB2E4\
  \uC591\uD55C \uBAA9\uC801\uC744 \uC81C\uACF5\uD558\uBA70 \uBB38\uC790\uC5F4\uC744\
  \ \uCC98\uB9AC\uD558\uB294 \uC720\uC5F0\uC131\uC744 \uC81C\uACF5\uD569\uB2C8\uB2E4\
  . `substring()` \uBA54\uC11C\uB4DC\uB294 \uAC04\uB2E8\uD569\uB2C8\uB2E4. \uC2DC\uC791\
  \u2026"
lastmod: '2024-03-13T22:44:54.769019-06:00'
model: gpt-4-0125-preview
summary: "Dart\uC5D0\uC11C\uB294 `substring()`, `split()`, \uC815\uADDC \uD45C\uD604\
  \uC2DD\uACFC \uAC19\uC740 \uB2E4\uC591\uD55C \uBA54\uC11C\uB4DC\uB97C \uC0AC\uC6A9\
  \uD558\uC5EC \uBD80\uBD84 \uBB38\uC790\uC5F4\uC744 \uCD94\uCD9C\uD560 \uC218 \uC788\
  \uC2B5\uB2C8\uB2E4."
title: "\uBD80\uBD84 \uBB38\uC790\uC5F4 \uCD94\uCD9C\uD558\uAE30"
weight: 6
---

## 어떻게:
Dart에서는 `substring()`, `split()`, 정규 표현식과 같은 다양한 메서드를 사용하여 부분 문자열을 추출할 수 있습니다. 각 메서드는 다양한 목적을 제공하며 문자열을 처리하는 유연성을 제공합니다.

### `substring()` 사용하기:
`substring()` 메서드는 간단합니다. 시작 인덱스(그리고 선택적으로 종료 인덱스)를 지정하여 문자열을 분할합니다.

```dart
void main() {
  String example = "Hello, World!";
  String result = example.substring(7, 12);
  print(result); // 출력: World
}
```

### `split()` 사용하기:
문자열을 패턴(공백이나 쉼표 같은)을 기준으로 부분 문자열 리스트로 분할하고, 그 다음 인덱스로 부분 문자열을 접근합니다.

```dart
void main() {
  String example = "Dart is fun";
  List<String> parts = example.split(' ');
  String result = parts[1]; // 인덱스로 접근
  print(result); // 출력: is
}
```

### 정규 표현식 사용하기:
복잡한 패턴의 경우, Dart의 `RegExp` 클래스는 강력합니다. 이를 사용하여 패턴을 매치하고 부분 문자열을 추출합니다.

```dart
void main() {
  String example = "Email: example@mail.com";
  RegExp regExp = RegExp(r"\b\w+@\w+\.\w+\b");
  String email = regExp.stringMatch(example)!;
  print(email); // 출력: example@mail.com
}
```

### 서드파티 라이브러리:
Dart의 표준 라이브러리는 상당히 유능하지만, 특정 작업을 단순화할 수 있는 서드파티 라이브러리를 만날 수 있는 상황이 있을 수 있습니다. 문자열 조작 및 패턴 매칭에 대해 특별히 지지하는 선호 라이브러리는 없지만 Dart의 내장 기능이 종종 충분하다고 여겨집니다. 그럼에도 특정 필요에 더 잘 맞는 라이브러리가 있는지 항상 [pub.dev](https://pub.dev)를 확인하세요.
