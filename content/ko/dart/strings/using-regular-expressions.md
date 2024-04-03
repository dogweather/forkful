---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:03.228007-07:00
description: "\uC0AC\uC6A9 \uBC29\uBC95: Dart\uC5D0\uC11C\uB294 `RegExp` \uD074\uB798\
  \uC2A4\uB97C \uC815\uADDC \uD45C\uD604\uC2DD\uC5D0 \uC0AC\uC6A9\uD569\uB2C8\uB2E4\
  . \uC5EC\uAE30 \uBB38\uC790\uC5F4 \uB0B4\uC5D0\uC11C \uAC04\uB2E8\uD55C \uD328\uD134\
  \uC744 \uC77C\uCE58\uC2DC\uD0A4\uB294 \uAE30\uBCF8 \uC608\uC81C\uAC00 \uC788\uC2B5\
  \uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:54.770825-06:00'
model: gpt-4-0125-preview
summary: "Dart\uC5D0\uC11C\uB294 `RegExp` \uD074\uB798\uC2A4\uB97C \uC815\uADDC \uD45C\
  \uD604\uC2DD\uC5D0 \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
title: "\uC815\uADDC \uD45C\uD604\uC2DD \uC0AC\uC6A9\uD558\uAE30"
weight: 11
---

## 사용 방법:
Dart에서는 `RegExp` 클래스를 정규 표현식에 사용합니다. 여기 문자열 내에서 간단한 패턴을 일치시키는 기본 예제가 있습니다:

```dart
void main() {
  var pattern = RegExp(r'\bDart\b');
  var text = 'Learning Dart programming is exciting.';

  if (pattern.hasMatch(text)) {
    print('Match found!');
  } else {
    print('No match found.');
  }
  // 출력: Match found!
}
```

문자열에서 일치하는 항목을 추출하려면 `allMatches` 메소드를 사용할 수 있습니다. 이 메소드는 일치하는 항목의 반복 가능한 객체를 반환합니다:

```dart
void main() {
  var pattern = RegExp(r'\b\w+\b');
  var text = 'Dart is awesome!';

  var matches = pattern.allMatches(text);
  for (final match in matches) {
    print(match.group(0)); // 이는 일치하는 부분 문자열을 출력합니다.
  }
  // 출력:
  // Dart
  // is
  // awesome
}
```

텍스트 교체는 `replaceFirst` 또는 `replaceAll` 메소드를 사용하여 수행할 수 있습니다:

```dart
void main() {
  var pattern = RegExp(r'\bDart\b');
  var text = 'Dart is not just a dart.';
  
  // 첫 번째 발생을 교체
  var modifiedText = text.replaceFirst(pattern, 'Flutter');
  print(modifiedText); 
  // 출력: Flutter is not just a dart.

  // 모든 발생을 교체
  modifiedText = text.replaceAll(pattern, 'Flutter');
  print(modifiedText);
  // 출력: Flutter is not just a flutter.
}
```

정규 표현식 패턴으로 문자열을 분할하는 것은 `split` 메소드를 사용하여 간단합니다:

```dart
void main() {
  var pattern = RegExp(r'\s+'); // 공백 문자와 일치
  var text = 'Dart is fun';

  var parts = text.split(pattern);
  print(parts); 
  // 출력: [Dart, is, fun]
}
```

Dart의 `RegExp`에서 직접 지원하지 않는 복잡한 파싱 또는 유효성 검사가 필요한 경우, 제삼자 라이브러리를 고려할 수 있지만, 일반적인 정규 표현식 작업에 충분한 Dart의 표준 라이브러리는 정규 표현식을 다루는 데 있어 그 유용성과 다양성을 강조합니다.
