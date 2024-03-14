---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:03.228007-07:00
description: "Dart\uC5D0\uC11C \uC815\uADDC \uD45C\uD604\uC2DD(regex)\uC740 \uBB38\
  \uC790\uC5F4 \uAC80\uC0C9 \uBC0F \uC870\uC791\uC744 \uC704\uD55C \uAC15\uB825\uD55C\
  \ \uBC29\uBC95\uC744 \uC81C\uACF5\uD558\uC5EC, \uD504\uB85C\uADF8\uB798\uBA38\uAC00\
  \ \uBCF5\uC7A1\uD55C \uD14D\uC2A4\uD2B8 \uCC98\uB9AC \uC791\uC5C5\uC744 \uD6A8\uC728\
  \uC801\uC73C\uB85C \uC218\uD589\uD560 \uC218 \uC788\uAC8C \uD569\uB2C8\uB2E4. \uC815\
  \uADDC \uD45C\uD604\uC2DD\uC744 \uC774\uD574\uD568\uC73C\uB85C\uC368 \uAC1C\uBC1C\
  \uC790\uB294 \uD14D\uC2A4\uD2B8 \uC720\uD6A8\uC131 \uAC80\uC0AC, \uAC80\uC0C9 \uD328\
  \uD134, \uD14D\uC2A4\uD2B8 \uBCC0\uD658\uC744 \uBE60\uB974\uAC8C \uC2E4\uD589\uD560\
  \ \uC218\u2026"
lastmod: '2024-03-13T22:44:54.770825-06:00'
model: gpt-4-0125-preview
summary: "Dart\uC5D0\uC11C \uC815\uADDC \uD45C\uD604\uC2DD(regex)\uC740 \uBB38\uC790\
  \uC5F4 \uAC80\uC0C9 \uBC0F \uC870\uC791\uC744 \uC704\uD55C \uAC15\uB825\uD55C \uBC29\
  \uBC95\uC744 \uC81C\uACF5\uD558\uC5EC, \uD504\uB85C\uADF8\uB798\uBA38\uAC00 \uBCF5\
  \uC7A1\uD55C \uD14D\uC2A4\uD2B8 \uCC98\uB9AC \uC791\uC5C5\uC744 \uD6A8\uC728\uC801\
  \uC73C\uB85C \uC218\uD589\uD560 \uC218 \uC788\uAC8C \uD569\uB2C8\uB2E4. \uC815\uADDC\
  \ \uD45C\uD604\uC2DD\uC744 \uC774\uD574\uD568\uC73C\uB85C\uC368 \uAC1C\uBC1C\uC790\
  \uB294 \uD14D\uC2A4\uD2B8 \uC720\uD6A8\uC131 \uAC80\uC0AC, \uAC80\uC0C9 \uD328\uD134\
  , \uD14D\uC2A4\uD2B8 \uBCC0\uD658\uC744 \uBE60\uB974\uAC8C \uC2E4\uD589\uD560 \uC218\
  \u2026"
title: "\uC815\uADDC \uD45C\uD604\uC2DD \uC0AC\uC6A9\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇인가 & 왜인가?
Dart에서 정규 표현식(regex)은 문자열 검색 및 조작을 위한 강력한 방법을 제공하여, 프로그래머가 복잡한 텍스트 처리 작업을 효율적으로 수행할 수 있게 합니다. 정규 표현식을 이해함으로써 개발자는 텍스트 유효성 검사, 검색 패턴, 텍스트 변환을 빠르게 실행할 수 있으며, 이는 현대 애플리케이션에서 폼 처리, 데이터 파싱, 일반 문자열 조작에 필수적입니다.

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
