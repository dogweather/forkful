---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:44.242667-07:00
description: "\uBC29\uBC95: Dart\uB294 \uC678\uBD80 \uB77C\uC774\uBE0C\uB7EC\uB9AC\
  \uAC00 \uD544\uC694 \uC5C6\uC774 \uC9C1\uC811 `String` \uD074\uB798\uC2A4\uB97C\
  \ \uD1B5\uD574 \uD14D\uC2A4\uD2B8 \uAC80\uC0C9 \uBC0F \uAD50\uCCB4\uB97C \uC704\uD55C\
  \ \uAC15\uB825\uD55C \uBC29\uBC95\uC744 \uC81C\uACF5\uD569\uB2C8\uB2E4. \uB2E4\uC74C\
  \uC740 \uADF8 \uBC29\uBC95\uC785\uB2C8\uB2E4: #."
lastmod: '2024-03-13T22:44:54.762135-06:00'
model: gpt-4-0125-preview
summary: "Dart\uB294 \uC678\uBD80 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uAC00 \uD544\uC694\
  \ \uC5C6\uC774 \uC9C1\uC811 `String` \uD074\uB798\uC2A4\uB97C \uD1B5\uD574 \uD14D\
  \uC2A4\uD2B8 \uAC80\uC0C9 \uBC0F \uAD50\uCCB4\uB97C \uC704\uD55C \uAC15\uB825\uD55C\
  \ \uBC29\uBC95\uC744 \uC81C\uACF5\uD569\uB2C8\uB2E4."
title: "\uD14D\uC2A4\uD2B8 \uAC80\uC0C9 \uBC0F \uAD50\uCCB4"
weight: 10
---

## 방법:
Dart는 외부 라이브러리가 필요 없이 직접 `String` 클래스를 통해 텍스트 검색 및 교체를 위한 강력한 방법을 제공합니다. 다음은 그 방법입니다:

### 기본 검색 및 교체
부분 문자열을 검색하여 다른 문자열로 교체하려면 `replaceAll`을 사용할 수 있습니다:

```dart
String sampleText = "Hello, Dart! Dart is great.";
String modifiedText = sampleText.replaceAll("Dart", "Flutter");
print(modifiedText); // 출력: Hello, Flutter! Flutter is great.
```

### 정규 표현식 사용
더 복잡한 검색 및 교체 요구 사항의 경우, Dart는 `RegExp` 클래스를 통해 정규 표현식을 활용합니다. 이를 통해 문자열에서 패턴 일치와 교체를 수행할 수 있습니다:

```dart
String sampleText = "Dart 2023, Flutter 2023";
String modifiedText = sampleText.replaceAll(RegExp(r'\d+'), "2024");
print(modifiedText); // 출력: Dart 2024, Flutter 2024
```

이 예시는 문자열 내의 하나 이상의 숫자 (`\d+`)를 찾아 "2024"로 교체합니다.

### 대소문자 구분 없는 검색
대소문자를 구분하지 않는 검색을 수행하려면 `RegExp` 생성자를 수정하여 대소문자를 무시하도록 할 수 있습니다:

```dart
String sampleText = "Welcome to Dart, the programming language.";
String modifiedText = sampleText.replaceAll(RegExp(r'dart', caseSensitive: false), "Flutter");
print(modifiedText); // 출력: Welcome to Flutter, the programming language.
```

### 함수를 사용한 교체
일치 항목 자체를 기반으로 동적 교체를 수행하려면 `replaceAllMapped`에 함수를 전달할 수 있습니다. 이 함수는 일치된 시퀀스에서 작업이나 계산을 수행할 수 있습니다:

```dart
String sampleText = "Increment 5 by 1 to get 6.";
String incrementedText = sampleText.replaceAllMapped(RegExp(r'\d+'), (Match m) => (int.parse(m[0]!) + 1).toString());
print(incrementedText); // 출력: Increment 6 by 1 to get 7.
```

이는 각 숫자 시퀀스를 증가된 값으로 교체합니다. 각 일치 항목은 정수로 변환되고 증가된 후 교체를 위해 다시 문자열로 변환됩니다.

Dart의 문자열 조작 능력, 특히 텍스트 검색 및 교체는 애플리케이션 내의 데이터 처리 및 준비 작업을 위한 강력한 도구로 만듭니다. 단순 문자열 교체를 사용하거나 정규 표현식의 힘을 활용하든, Dart는 효과적인 텍스트 조작을 위한 필요한 유연성과 성능을 제공합니다.
