---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:49.897944-07:00
description: "\uBC29\uBC95: Dart\uB294 \uB0B4\uC7A5 REPL\uC744 \uC81C\uACF5\uD558\uC9C0\
  \ \uC54A\uC2B5\uB2C8\uB2E4. \uD558\uC9C0\uB9CC, DartPad(\uC628\uB77C\uC778)\uC744\
  \ \uC0AC\uC6A9\uD558\uAC70\uB098 `dart_repl` \uAC19\uC740 \uC81C3\uC790 \uB3C4\uAD6C\
  \uB97C \uD65C\uC6A9\uD558\uC5EC REPL\uACFC \uC720\uC0AC\uD55C \uAE30\uB2A5\uC744\
  \ \uB2EC\uC131\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4. **DartPad \uC0AC\uC6A9\uD558\
  \uAE30:**\u2026"
lastmod: '2024-03-13T22:44:54.791018-06:00'
model: gpt-4-0125-preview
summary: "Dart\uB294 \uB0B4\uC7A5 REPL\uC744 \uC81C\uACF5\uD558\uC9C0 \uC54A\uC2B5\
  \uB2C8\uB2E4."
title: "\uB300\uD654\uD615 \uC258 (REPL) \uC0AC\uC6A9\uD558\uAE30"
weight: 34
---

## 방법:
Dart는 내장 REPL을 제공하지 않습니다. 하지만, DartPad(온라인)을 사용하거나 `dart_repl` 같은 제3자 도구를 활용하여 REPL과 유사한 기능을 달성할 수 있습니다.

**DartPad 사용하기:**

DartPad(https://dartpad.dev)은 웹 브라우저에서 Dart 코드를 작성하고 실행할 수 있는 온라인 Dart 에디터입니다. 전통적인 커맨드 라인 REPL은 아니지만, 빠른 실험을 위해 비슷한 경험을 제공합니다.

웹사이트로 이동하여 왼쪽 패널에 Dart 코드를 입력한 다음 "실행"을 클릭하여 오른쪽에 출력을 봅니다.

예시:
```dart
void main() {
  print('Hello, Dart!');
}
```
출력:
```
Hello, Dart!
```

**`dart_repl` 사용하기 (제3자 도구):**

먼저, pub을 통해 전역적으로 `dart_repl`을 설치합니다:

```shell
dart pub global activate dart_repl
```

그다음, 터미널에서 `dart_repl`을 실행합니다:

```shell
dart_repl
```

이제, 셸에 바로 Dart 문장을 입력하기 시작할 수 있습니다. 예를 들면:

```dart
>>> print('Hello, REPL!');
Hello, REPL!
>>> int add(int x, int y) => x + y;
>>> print(add(5, 7));
12
```

이 방법들은 즉석에서 Dart 코드를 시도해 보는 빠른 방법을 제공하여 학습 곡선을 크게 완화시키고 생산성을 향상시킵니다.
