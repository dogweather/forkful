---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:53:32.278954-07:00
description: "\uBB38\uC790\uC5F4\uC758 \uCCAB \uAE00\uC790\uB97C \uB300\uBB38\uC790\
  \uB85C \uBC14\uAFB8\uACE0 \uB098\uBA38\uC9C0 \uBB38\uC790\uB294 \uADF8\uB300\uB85C\
  \ \uC720\uC9C0\uD558\uB294 \uAC83\uC744 \uBB38\uC790\uC5F4\uC744 \uB300\uBB38\uC790\
  \uD654\uD55C\uB2E4\uACE0 \uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\
  \uC740 \uC0AC\uC6A9\uC790 \uC785\uB825\uC758 \uD615\uC2DD\uC744 \uB9DE\uCD94\uAC70\
  \uB098 \uC0AC\uC6A9\uC790 \uC778\uD130\uD398\uC774\uC2A4\uC5D0\uC11C \uBB38\uBC95\
  \ \uADDC\uCE59\uC744 \uC900\uC218\uD558\uAE30 \uC704\uD574 \uD14D\uC2A4\uD2B8\uB97C\
  \ \uD45C\uC2DC\uD560 \uB54C \uC885\uC885 \uC774 \uAE30\uC220\uC744 \uC0AC\uC6A9\uD569\
  \uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:54.758814-06:00'
model: gpt-4-0125-preview
summary: "\uBB38\uC790\uC5F4\uC758 \uCCAB \uAE00\uC790\uB97C \uB300\uBB38\uC790\uB85C\
  \ \uBC14\uAFB8\uACE0 \uB098\uBA38\uC9C0 \uBB38\uC790\uB294 \uADF8\uB300\uB85C \uC720\
  \uC9C0\uD558\uB294 \uAC83\uC744 \uBB38\uC790\uC5F4\uC744 \uB300\uBB38\uC790\uD654\
  \uD55C\uB2E4\uACE0 \uD569\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uB300\uBB38\uC790\uD654"
weight: 2
---

## 무엇 & 왜?

문자열의 첫 글자를 대문자로 바꾸고 나머지 문자는 그대로 유지하는 것을 문자열을 대문자화한다고 합니다. 프로그래머들은 사용자 입력의 형식을 맞추거나 사용자 인터페이스에서 문법 규칙을 준수하기 위해 텍스트를 표시할 때 종종 이 기술을 사용합니다.

## 방법:

### Dart의 내장 메소드 사용하기

Dart는 문자열 조작을 위한 간단하고 직관적인 메소드를 제공합니다. 단어나 문장을 대문자화하려면 일반적으로 첫 문자를 대문자로 변환한 다음 나머지 문자열과 연결합니다. 다음은 그 방법을 구현하는 방법입니다:

```dart
String capitalize(String text) {
  if (text.isEmpty) return text;
  return text[0].toUpperCase() + text.substring(1).toLowerCase();
}

void main() {
  var example = "hello world";
  print(capitalize(example)); // 출력: Hello world
}
```

### 각 단어의 첫 글자 대문자화하기

문자열의 각 단어의 첫 글자를 대문자로 바꾸려면, 문자열을 단어로 나누고, 각각을 대문자화한 다음 다시 합칠 수 있습니다:

```dart
String capitalizeWords(String text) {
  return text.split(' ').map(capitalize).join(' ');
}

void main() {
  var example = "hello dart enthusiasts";
  print(capitalizeWords(example)); // 출력: Hello Dart Enthusiasts
}
```

### 타사 라이브러리 사용하기

Dart의 표준 라이브러리가 기본적인 요구 사항을 충족시키지만, 특정 작업은 타사 패키지를 사용하여 더 편리하게 수행될 수 있습니다. 확장된 문자열 조작 기능, 포함하여 대문자화 기능을 제공하는 인기 있는 선택은 [`recase`](https://pub.dev/packages/recase) 패키지입니다. 프로젝트의 `pubspec.yaml`에 추가한 후, 여러 기능 중 문자열을 쉽게 대문자화할 수 있습니다:

```dart
import 'package:recase/recase.dart';

void main() {
  var example = "hello world";
  var rc = ReCase(example);

  print(rc.titleCase); // 출력: Hello World
}
```

`recase`를 사용하면, 문자열 변환을 수동으로 처리하지 않고도 개별 단어, 전체 문장을 대문자화하거나 다른 케이싱 규칙을 따를 수 있습니다.
