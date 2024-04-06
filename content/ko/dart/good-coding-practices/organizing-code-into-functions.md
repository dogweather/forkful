---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:18.956362-07:00
description: "\uBC29\uBC95: Dart\uC5D0\uC11C \uD568\uC218\uB97C \uC815\uC758\uD560\
  \ \uB54C \uBC18\uD658 \uAC12\uC774 \uC5C6\uC73C\uBA74 `void` \uD0A4\uC6CC\uB4DC\uB97C\
  \ \uC0AC\uC6A9\uD558\uAC70\uB098, \uADF8\uB807\uC9C0 \uC54A\uC73C\uBA74 \uBC18\uD658\
  \ \uAC12\uC758 \uC720\uD615\uC744 \uC9C0\uC815\uD569\uB2C8\uB2E4. \uB2E4\uC74C\uC740\
  \ \uC778\uC0AC \uBA54\uC2DC\uC9C0\uB97C \uCD9C\uB825\uD558\uB294 \uAC04\uB2E8\uD55C\
  \ \uD568\uC218\uC785\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:54.797683-06:00'
model: gpt-4-0125-preview
summary: "Dart\uC5D0\uC11C \uD568\uC218\uB97C \uC815\uC758\uD560 \uB54C \uBC18\uD658\
  \ \uAC12\uC774 \uC5C6\uC73C\uBA74 `void` \uD0A4\uC6CC\uB4DC\uB97C \uC0AC\uC6A9\uD558\
  \uAC70\uB098, \uADF8\uB807\uC9C0 \uC54A\uC73C\uBA74 \uBC18\uD658 \uAC12\uC758 \uC720\
  \uD615\uC744 \uC9C0\uC815\uD569\uB2C8\uB2E4."
title: "\uCF54\uB4DC\uB97C \uD568\uC218\uB85C \uAD6C\uC131\uD558\uAE30"
weight: 18
---

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
