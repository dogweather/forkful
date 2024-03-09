---
title:                "대화형 쉘 (REPL) 사용하기"
date:                  2024-03-08T21:56:49.897944-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?

Dart용 대화형 셸(REPL - 읽기-평가-출력 반복)은 프로그래머가 전체 스크립트를 컴파일할 필요 없이 Dart 코드를 줄 단위로 동적으로 입력하고 실행할 수 있게 해줍니다. 이 도구는 Dart의 문법을 배우고, 코드 조각을 실험하며, 즉각적인 피드백과 반복 테스트를 용이하게 함으로써 디버깅할 때 귀중한 도움이 됩니다.

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
