---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:53:57.948485-07:00
description: "\uBC29\uBC95: Dart\uB294 \uBB38\uC790\uC5F4\uC744 \uC5F0\uACB0\uD558\
  \uB294 \uBA87 \uAC00\uC9C0 \uAC04\uB2E8\uD55C \uBC29\uBC95\uC744 \uC81C\uACF5\uD569\
  \uB2C8\uB2E4. \uC544\uB798\uB294 \uAC00\uC7A5 \uC77C\uBC18\uC801\uC778 \uBC29\uBC95\
  \uB4E4\uC785\uB2C8\uB2E4: #."
lastmod: '2024-03-13T22:44:54.774230-06:00'
model: gpt-4-0125-preview
summary: "Dart\uB294 \uBB38\uC790\uC5F4\uC744 \uC5F0\uACB0\uD558\uB294 \uBA87 \uAC00\
  \uC9C0 \uAC04\uB2E8\uD55C \uBC29\uBC95\uC744 \uC81C\uACF5\uD569\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uC5F0\uACB0\uD558\uAE30"
weight: 3
---

## 방법:
Dart는 문자열을 연결하는 몇 가지 간단한 방법을 제공합니다. 아래는 가장 일반적인 방법들입니다:

### `+` 연산자 사용하기
`+` 연산자는 문자열을 결합하는 가장 직관적인 방법입니다.
```dart
String greeting = 'Hello, ' + 'World!';
print(greeting); // 출력: Hello, World!
```

### `concat()` 메소드 사용하기
다른 언어와 비슷한 `concat()` 메소드는 Dart에는 없지만, `+`나 다음 방법을 사용하여 같은 결과를 얻을 수 있습니다.

### 문자열 내삽(String Interpolation) 사용하기
문자열 내삽을 사용하면 변수를 문자열 내에 직접 포함시킬 수 있습니다. 문자열과 표현식을 결합하기에 효율적입니다.
```dart
String user = 'Jane';
String message = 'Welcome, $user!';
print(message); // 출력: Welcome, Jane!
```

### `join()` 메소드 사용하기
`join()` 메소드는 연결하고자 하는 문자열 목록이 있을 때 유용합니다.
```dart
var words = ['Hello', 'from', 'Dart'];
String sentence = words.join(' '); // 공백 구분자로 결합합니다.
print(sentence); // 출력: Hello from Dart
```

### StringBuffer 사용하기
`StringBuffer`는 특히 반복문에서 여러 번의 연결에 효율적입니다.
```dart
var words = ['Dart', 'is', 'fun'];
StringBuffer buffer = StringBuffer();
for (String word in words) {
  buffer.write(word); // 각 단어를 버퍼에 추가합니다.
  buffer.write(' '); // 선택적으로 공백을 추가합니다.
}
String sentence = buffer.toString().trim(); // 문자열로 변환하고 마지막 공백을 제거합니다.
print(sentence); // 출력: Dart is fun
```

### 타사 라이브러리
Dart의 표준 라이브러리는 일반적으로 문자열 연결 작업에 충분하지만, `quiver`와 같은 타사 라이브러리는 Dart의 내장 기능을 보완하는 유틸리티를 제공합니다. 예를 들어, 고급 시나리오를 위해 `quiver`의 `concat()` 또는 `merge()` 기능을 탐색할 수 있습니다. 하지만, Dart의 강력한 내장 옵션으로 충분하지 않은 특정한 필요가 있지 않는 한, 내장 옵션을 고수하세요.
