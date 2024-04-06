---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:57.473869-07:00
description: "\uBC29\uBC95: Dart\uB294 \uB0B4\uC7A5\uB41C \uBB38\uC790\uC5F4 \uBA54\
  \uC18C\uB4DC\uB97C \uC0AC\uC6A9\uD558\uC5EC \uC81C3\uC790 \uB77C\uC774\uBE0C\uB7EC\
  \uB9AC \uC5C6\uC774\uB3C4 \uBB38\uC790\uC5F4\uC5D0\uC11C \uB530\uC634\uD45C\uB97C\
  \ \uC81C\uAC70\uD560 \uC218 \uC788\uB294 \uAC04\uB2E8\uD55C \uBC29\uBC95\uC744 \uC81C\
  \uACF5\uD569\uB2C8\uB2E4. \uB530\uC634\uD45C\uB85C \uC2DC\uC791\uD558\uACE0 \uB05D\
  \uB098\uB294 \uBB38\uC790\uC5F4\uC744 \uB2E4\uB8E8\uACE0 \uC788\uB2E4\uBA74, `replaceFirst`\uC640\
  \ `replaceAll` \uBA54\uC18C\uB4DC\uB97C \uC0AC\uC6A9\uD558\uC5EC \uC81C\uAC70\uD560\
  \ \uC218 \uC788\uC2B5\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:54.767332-06:00'
model: gpt-4-0125-preview
summary: "Dart\uB294 \uB0B4\uC7A5\uB41C \uBB38\uC790\uC5F4 \uBA54\uC18C\uB4DC\uB97C\
  \ \uC0AC\uC6A9\uD558\uC5EC \uC81C3\uC790 \uB77C\uC774\uBE0C\uB7EC\uB9AC \uC5C6\uC774\
  \uB3C4 \uBB38\uC790\uC5F4\uC5D0\uC11C \uB530\uC634\uD45C\uB97C \uC81C\uAC70\uD560\
  \ \uC218 \uC788\uB294 \uAC04\uB2E8\uD55C \uBC29\uBC95\uC744 \uC81C\uACF5\uD569\uB2C8\
  \uB2E4."
title: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB530\uC634\uD45C \uC81C\uAC70\uD558\uAE30"
weight: 9
---

## 방법:
Dart는 내장된 문자열 메소드를 사용하여 제3자 라이브러리 없이도 문자열에서 따옴표를 제거할 수 있는 간단한 방법을 제공합니다.

### 예제 1: `replaceFirst` 및 `replaceAll` 사용하기
따옴표로 시작하고 끝나는 문자열을 다루고 있다면, `replaceFirst`와 `replaceAll` 메소드를 사용하여 제거할 수 있습니다.

```dart
String quotedString = '"Hello, World!"';
String singleQuotedString = '\'Dart Programming\'';

// 큰따옴표 제거
String noDoubleQuotes = quotedString.replaceFirst('"', '').replaceAll('"', '');
print(noDoubleQuotes); // 출력: Hello, World!

// 작은따옴표 제거
String noSingleQuotes = singleQuotedString.replaceFirst('\'', '').replaceAll('\'', '');
print(noSingleQuotes); // 출력: Dart Programming
```

### 예제 2: `substring` 사용하기
문자열의 시작과 끝에 따옴표가 확실히 있는 경우에 유용한 메서드입니다.

```dart
String quotedString = '"Flutter Development"';
// 삭제하기 전에 따옴표로 시작하고 끝나는지 확인하여 오류를 방지하세요.
if (quotedString.startsWith('"') && quotedString.endsWith('"')) {
  quotedString = quotedString.substring(1, quotedString.length - 1);
}
print(quotedString); // 출력: Flutter Development
```

### 예제 3: 사용자 정의 확장 메소드
프로젝트에서 따옴표 제거 작업을 자주 수행해야 하는 경우, `String`에 대한 사용자 정의 확장을 만드는 것이 더 재사용성이 좋습니다.

```dart
extension UnquoteString on String {
  String unquote() {
    var str = this;
    if (str.startsWith('"') && str.endsWith('"') || str.startsWith('\'') && str.endsWith('\'')) {
      str = str.substring(1, str.length - 1);
    }
    return str;
  }
}

void main() {
  String doubleQuoted = '"This is Dart"';
  String singleQuoted = '\'This is awesome\'';
  print(doubleQuoted.unquote()); // 출력: This is Dart
  print(singleQuoted.unquote()); // 출력: This is awesome
}
```

이러한 접근 방식들을 통해 Dart에서 효과적으로 문자열에서 따옴표를 제거할 수 있으며, 데이터 처리 및 준비 워크플로우를 개선할 수 있습니다.
