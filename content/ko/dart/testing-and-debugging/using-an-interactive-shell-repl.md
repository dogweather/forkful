---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:49.897944-07:00
description: "Dart\uC6A9 \uB300\uD654\uD615 \uC178(REPL - \uC77D\uAE30-\uD3C9\uAC00\
  -\uCD9C\uB825 \uBC18\uBCF5)\uC740 \uD504\uB85C\uADF8\uB798\uBA38\uAC00 \uC804\uCCB4\
  \ \uC2A4\uD06C\uB9BD\uD2B8\uB97C \uCEF4\uD30C\uC77C\uD560 \uD544\uC694 \uC5C6\uC774\
  \ Dart \uCF54\uB4DC\uB97C \uC904 \uB2E8\uC704\uB85C \uB3D9\uC801\uC73C\uB85C \uC785\
  \uB825\uD558\uACE0 \uC2E4\uD589\uD560 \uC218 \uC788\uAC8C \uD574\uC90D\uB2C8\uB2E4\
  . \uC774 \uB3C4\uAD6C\uB294 Dart\uC758 \uBB38\uBC95\uC744 \uBC30\uC6B0\uACE0, \uCF54\
  \uB4DC \uC870\uAC01\uC744 \uC2E4\uD5D8\uD558\uBA70, \uC989\uAC01\uC801\uC778 \uD53C\
  \uB4DC\uBC31\uACFC \uBC18\uBCF5 \uD14C\uC2A4\uD2B8\uB97C\u2026"
lastmod: '2024-03-13T22:44:54.791018-06:00'
model: gpt-4-0125-preview
summary: "Dart\uC6A9 \uB300\uD654\uD615 \uC178(REPL - \uC77D\uAE30-\uD3C9\uAC00-\uCD9C\
  \uB825 \uBC18\uBCF5)\uC740 \uD504\uB85C\uADF8\uB798\uBA38\uAC00 \uC804\uCCB4 \uC2A4\
  \uD06C\uB9BD\uD2B8\uB97C \uCEF4\uD30C\uC77C\uD560 \uD544\uC694 \uC5C6\uC774 Dart\
  \ \uCF54\uB4DC\uB97C \uC904 \uB2E8\uC704\uB85C \uB3D9\uC801\uC73C\uB85C \uC785\uB825\
  \uD558\uACE0 \uC2E4\uD589\uD560 \uC218 \uC788\uAC8C \uD574\uC90D\uB2C8\uB2E4."
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
