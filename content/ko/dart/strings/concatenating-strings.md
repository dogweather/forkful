---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:53:57.948485-07:00
description: "\uD504\uB85C\uADF8\uB798\uBC0D\uC5D0\uC11C \uBB38\uC790\uC5F4\uC744\
  \ \uC5F0\uACB0\uD55C\uB2E4\uB294 \uAC83\uC740 \uB450 \uAC1C \uC774\uC0C1\uC758 \uBB38\
  \uC790\uC5F4\uC744 \uD558\uB098\uB85C \uACB0\uD569\uD558\uB294 \uAC83\uC744 \uB9D0\
  \uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC774\uB97C \uD1B5\
  \uD574 \uD14D\uC2A4\uD2B8 \uB370\uC774\uD130\uB97C \uC27D\uAC8C \uC870\uC791\uD558\
  \uAC70\uB098, \uBA54\uC2DC\uC9C0\uB97C \uAD6C\uC131\uD558\uAC70\uB098, \uC0AC\uC6A9\
  \uC790 \uC778\uD130\uD398\uC774\uC2A4\uC758 \uC77C\uBD80\uB97C \uB3D9\uC801\uC73C\
  \uB85C \uC870\uB9BD\uD558\uAE30 \uC704\uD574 \uC774 \uAE30\uB2A5\uC744 \uC0AC\uC6A9\
  \uD569\uB2C8\uB2E4."
lastmod: '2024-03-09T21:06:18.739871-07:00'
model: gpt-4-0125-preview
summary: "\uD504\uB85C\uADF8\uB798\uBC0D\uC5D0\uC11C \uBB38\uC790\uC5F4\uC744 \uC5F0\
  \uACB0\uD55C\uB2E4\uB294 \uAC83\uC740 \uB450 \uAC1C \uC774\uC0C1\uC758 \uBB38\uC790\
  \uC5F4\uC744 \uD558\uB098\uB85C \uACB0\uD569\uD558\uB294 \uAC83\uC744 \uB9D0\uD569\
  \uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC774\uB97C \uD1B5\uD574\
  \ \uD14D\uC2A4\uD2B8 \uB370\uC774\uD130\uB97C \uC27D\uAC8C \uC870\uC791\uD558\uAC70\
  \uB098, \uBA54\uC2DC\uC9C0\uB97C \uAD6C\uC131\uD558\uAC70\uB098, \uC0AC\uC6A9\uC790\
  \ \uC778\uD130\uD398\uC774\uC2A4\uC758 \uC77C\uBD80\uB97C \uB3D9\uC801\uC73C\uB85C\
  \ \uC870\uB9BD\uD558\uAE30 \uC704\uD574 \uC774 \uAE30\uB2A5\uC744 \uC0AC\uC6A9\uD569\
  \uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uC5F0\uACB0\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?
프로그래밍에서 문자열을 연결한다는 것은 두 개 이상의 문자열을 하나로 결합하는 것을 말합니다. 프로그래머들은 이를 통해 텍스트 데이터를 쉽게 조작하거나, 메시지를 구성하거나, 사용자 인터페이스의 일부를 동적으로 조립하기 위해 이 기능을 사용합니다.

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
