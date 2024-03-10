---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:18.956362-07:00
description: "Dart\uC5D0\uC11C \uD568\uC218\uB85C \uCF54\uB4DC\uB97C \uC870\uC9C1\uD654\
  \uD558\uB294 \uAC83\uC740 \uD2B9\uC815 \uC791\uC5C5\uC744 \uC218\uD589\uD558\uB294\
  \ \uC7AC\uC0AC\uC6A9 \uAC00\uB2A5\uD55C \uCF54\uB4DC \uBE14\uB85D\uC744 \uC815\uC758\
  \uD558\uB294 \uAC83\uACFC \uAD00\uB828\uC774 \uC788\uC2B5\uB2C8\uB2E4. \uC77C\uBC18\
  \uC801\uC73C\uB85C \uC785\uB825\uC744 \uBC1B\uC544, \uB370\uC774\uD130\uB97C \uCC98\
  \uB9AC\uD558\uACE0, \uAC00\uB2A5\uD558\uB2E4\uBA74 \uCD9C\uB825\uC744 \uBC18\uD658\
  \uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC774\uB97C \uD1B5\
  \uD574 \uCF54\uB4DC\uC758 \uAC00\uB3C5\uC131\uC744 \uB192\uC774\uACE0, \uC911\uBCF5\
  \uC744 \uC904\uC774\uBA70, \uC720\uC9C0 \uBCF4\uC218\uB97C\u2026"
lastmod: '2024-03-09T21:06:18.753534-07:00'
model: gpt-4-0125-preview
summary: "Dart\uC5D0\uC11C \uD568\uC218\uB85C \uCF54\uB4DC\uB97C \uC870\uC9C1\uD654\
  \uD558\uB294 \uAC83\uC740 \uD2B9\uC815 \uC791\uC5C5\uC744 \uC218\uD589\uD558\uB294\
  \ \uC7AC\uC0AC\uC6A9 \uAC00\uB2A5\uD55C \uCF54\uB4DC \uBE14\uB85D\uC744 \uC815\uC758\
  \uD558\uB294 \uAC83\uACFC \uAD00\uB828\uC774 \uC788\uC2B5\uB2C8\uB2E4. \uC77C\uBC18\
  \uC801\uC73C\uB85C \uC785\uB825\uC744 \uBC1B\uC544, \uB370\uC774\uD130\uB97C \uCC98\
  \uB9AC\uD558\uACE0, \uAC00\uB2A5\uD558\uB2E4\uBA74 \uCD9C\uB825\uC744 \uBC18\uD658\
  \uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC774\uB97C \uD1B5\
  \uD574 \uCF54\uB4DC\uC758 \uAC00\uB3C5\uC131\uC744 \uB192\uC774\uACE0, \uC911\uBCF5\
  \uC744 \uC904\uC774\uBA70, \uC720\uC9C0 \uBCF4\uC218\uB97C\u2026"
title: "\uCF54\uB4DC\uB97C \uD568\uC218\uB85C \uAD6C\uC131\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇을, 왜?
Dart에서 함수로 코드를 조직화하는 것은 특정 작업을 수행하는 재사용 가능한 코드 블록을 정의하는 것과 관련이 있습니다. 일반적으로 입력을 받아, 데이터를 처리하고, 가능하다면 출력을 반환합니다. 프로그래머들은 이를 통해 코드의 가독성을 높이고, 중복을 줄이며, 유지 보수를 용이하게 하여, 결국 더 모듈화되고 관리하기 쉬운 코드베이스로 이어지게 합니다.

## 방법:
### 기본 함수
Dart에서 함수를 정의할 때 반환 값이 없으면 `void` 키워드를 사용하거나, 그렇지 않으면 반환 값의 유형을 지정합니다. 다음은 인사 메시지를 출력하는 간단한 함수입니다:

```dart
void greet(String name) {
  print('안녕, $name!');
}

void main() {
  greet('앨리스');  // 출력: 안녕, 앨리스!
}
```

### 값을 반환하는 함수
함수는 값을 반환할 수 있습니다. 다음 예제는 두 정수를 입력으로 받아 그 합을 반환합니다:

```dart
int add(int a, int b) {
  return a + b;
}

void main() {
  var sum = add(5, 3);
  print(sum);  // 출력: 8
}
```

### 익명 함수
Dart는 람다 표현식 또는 클로저라고도 알려진 익명 함수를 지원합니다. 이는 즉석에서 짧은 기능을 위해 유용할 수 있습니다. 다음은 리스트의 `forEach` 메소드와 함께 익명 함수를 사용하는 방법입니다:

```dart
void main() {
  var fruits = ['사과', '바나나', '체리'];
  fruits.forEach((item) {
    print(item);
  });
  // 출력:
  // 사과
  // 바나나
  // 체리
}
```

### 단일 표현식 함수를 위한 화살표 구문
단일 표현식만을 포함하는 함수의 경우, Dart는 "화살표" 표기법(`=>`)을 사용하는 간결한 구문을 제공합니다. 이는 짧은 함수나 함수를 인자로 전달할 때 특히 유용합니다:

```dart
int square(int num) => num * num;

void main() {
  print(square(4));  // 출력: 16
}
```

### 서드파티 라이브러리 사용하기
더 복잡하거나 특화된 기능에 대해, Dart 프로그래머들은 종종 서드파티 라이브러리에 의존합니다. 예를 들어 HTTP 요청을 만드는 데 `http` 라이브러리를 고려해보세요. 첫 번째로, 의존성 아래에 `http`를 pubspec.yaml 파일에 추가하세요:

```
dependencies:
  http: ^0.13.3
```

그런 다음, 웹에서 데이터를 가져오는 데 사용할 수 있습니다:

```dart
import 'package:http/http.dart' as http;

Future<void> fetchUserData() async {
  var response = await http.get(Uri.parse('https://api.example.com/users/1'));
  print(response.body);
}

void main() {
  fetchUserData();
  // 예상 출력: 사용자의 JSON 데이터. 실제 출력은 API의 응답에 따라 다를 수 있습니다.
}
```

Dart 코드를 함수로 조직화할 때, 재사용성, 명확성 및 단일 책임 원칙에 대해 생각하세요. 이는 코드를 더 깨끗하게 만들 뿐만 아니라, 다른 사람들(그리고 미래의 당신)이 이해하고 유지 보수하기 쉽게 합니다.
