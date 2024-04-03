---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:53:32.278954-07:00
description: "\uBC29\uBC95: #."
lastmod: '2024-03-13T22:44:54.758814-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "\uBB38\uC790\uC5F4 \uB300\uBB38\uC790\uD654"
weight: 2
---

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
