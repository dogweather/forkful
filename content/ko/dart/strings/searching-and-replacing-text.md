---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:44.242667-07:00
description: "Dart\uC5D0\uC11C \uD14D\uC2A4\uD2B8 \uAC80\uC0C9 \uBC0F \uAD50\uCCB4\
  \uB294 \uBB38\uC790\uC5F4\uC744 \uAC80\uD1A0\uD558\uC5EC \uD2B9\uC815 \uD328\uD134\
  \uC774\uB098 \uBB38\uC790 \uC2DC\uD000\uC2A4\uB97C \uCC3E\uACE0 \uC774\uB97C \uC0C8\
  \uB85C\uC6B4 \uB0B4\uC6A9\uC73C\uB85C \uB300\uCCB4\uD558\uB294 \uC791\uC5C5\uC744\
  \ \uD3EC\uD568\uD569\uB2C8\uB2E4. \uC774 \uC791\uC5C5\uC740 \uB370\uC774\uD130 \uC720\
  \uD6A8\uC131 \uAC80\uC0AC, \uCD9C\uB825 \uD3EC\uB9F7\uD305, \uC0AC\uC6A9\uC790 \uC785\
  \uB825 \uD30C\uC2F1 \uB610\uB294 URL \uBC0F \uD30C\uC77C \uACBD\uB85C \uC870\uC791\
  \uACFC \uAC19\uC740 \uC791\uC5C5\uC5D0 \uAE30\uBCF8\uC801\uC774\uBA70, \uC560\uD50C\
  \uB9AC\uCF00\uC774\uC158\uC744\u2026"
lastmod: '2024-03-13T22:44:54.762135-06:00'
model: gpt-4-0125-preview
summary: "Dart\uC5D0\uC11C \uD14D\uC2A4\uD2B8 \uAC80\uC0C9 \uBC0F \uAD50\uCCB4\uB294\
  \ \uBB38\uC790\uC5F4\uC744 \uAC80\uD1A0\uD558\uC5EC \uD2B9\uC815 \uD328\uD134\uC774\
  \uB098 \uBB38\uC790 \uC2DC\uD000\uC2A4\uB97C \uCC3E\uACE0 \uC774\uB97C \uC0C8\uB85C\
  \uC6B4 \uB0B4\uC6A9\uC73C\uB85C \uB300\uCCB4\uD558\uB294 \uC791\uC5C5\uC744 \uD3EC\
  \uD568\uD569\uB2C8\uB2E4. \uC774 \uC791\uC5C5\uC740 \uB370\uC774\uD130 \uC720\uD6A8\
  \uC131 \uAC80\uC0AC, \uCD9C\uB825 \uD3EC\uB9F7\uD305, \uC0AC\uC6A9\uC790 \uC785\uB825\
  \ \uD30C\uC2F1 \uB610\uB294 URL \uBC0F \uD30C\uC77C \uACBD\uB85C \uC870\uC791\uACFC\
  \ \uAC19\uC740 \uC791\uC5C5\uC5D0 \uAE30\uBCF8\uC801\uC774\uBA70, \uC560\uD50C\uB9AC\
  \uCF00\uC774\uC158\uC744\u2026"
title: "\uD14D\uC2A4\uD2B8 \uAC80\uC0C9 \uBC0F \uAD50\uCCB4"
---

{{< edit_this_page >}}

## 무엇 & 왜?

Dart에서 텍스트 검색 및 교체는 문자열을 검토하여 특정 패턴이나 문자 시퀀스를 찾고 이를 새로운 내용으로 대체하는 작업을 포함합니다. 이 작업은 데이터 유효성 검사, 출력 포맷팅, 사용자 입력 파싱 또는 URL 및 파일 경로 조작과 같은 작업에 기본적이며, 애플리케이션을 더 동적이고 사용자의 요구에 반응적으로 만듭니다.

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
