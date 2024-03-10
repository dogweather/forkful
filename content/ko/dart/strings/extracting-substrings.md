---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:55.499953-07:00
description: "\uBD80\uBD84 \uBB38\uC790\uC5F4\uC744 \uCD94\uCD9C\uD558\uB294 \uAC83\
  \uC740 \uADF8\uB4E4\uC758 \uC704\uCE58\uB098 \uD328\uD134\uC744 \uAE30\uBC18\uC73C\
  \uB85C \uBB38\uC790\uC5F4\uC758 \uD2B9\uC815 \uBD80\uBD84\uC744 \uAC80\uC0C9\uD558\
  \uB294 \uAC83\uC5D0 \uB300\uD55C \uAC83\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\
  \uBA38\uB4E4\uC740 \uC0AC\uC6A9\uC790 \uC785\uB825\uC744 \uAD6C\uBB38 \uBD84\uC11D\
  \uD558\uAC70\uB098, \uB370\uC774\uD130\uB97C \uC870\uC791\uD558\uAC70\uB098, \uB354\
  \ \uD070 \uD14D\uC2A4\uD2B8 \uC18C\uC2A4\uC5D0\uC11C \uAD00\uB828 \uC815\uBCF4\uB97C\
  \ \uCD94\uCD9C\uD558\uB294 \uB4F1\uC758 \uC791\uC5C5\uC744 \uC704\uD574 \uC774\uB97C\
  \ \uC218\uD589\uD569\uB2C8\uB2E4."
lastmod: '2024-03-09T21:06:18.737041-07:00'
model: gpt-4-0125-preview
summary: "\uBD80\uBD84 \uBB38\uC790\uC5F4\uC744 \uCD94\uCD9C\uD558\uB294 \uAC83\uC740\
  \ \uADF8\uB4E4\uC758 \uC704\uCE58\uB098 \uD328\uD134\uC744 \uAE30\uBC18\uC73C\uB85C\
  \ \uBB38\uC790\uC5F4\uC758 \uD2B9\uC815 \uBD80\uBD84\uC744 \uAC80\uC0C9\uD558\uB294\
  \ \uAC83\uC5D0 \uB300\uD55C \uAC83\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\
  \uB4E4\uC740 \uC0AC\uC6A9\uC790 \uC785\uB825\uC744 \uAD6C\uBB38 \uBD84\uC11D\uD558\
  \uAC70\uB098, \uB370\uC774\uD130\uB97C \uC870\uC791\uD558\uAC70\uB098, \uB354 \uD070\
  \ \uD14D\uC2A4\uD2B8 \uC18C\uC2A4\uC5D0\uC11C \uAD00\uB828 \uC815\uBCF4\uB97C \uCD94\
  \uCD9C\uD558\uB294 \uB4F1\uC758 \uC791\uC5C5\uC744 \uC704\uD574 \uC774\uB97C \uC218\
  \uD589\uD569\uB2C8\uB2E4."
title: "\uBD80\uBD84 \uBB38\uC790\uC5F4 \uCD94\uCD9C\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?
부분 문자열을 추출하는 것은 그들의 위치나 패턴을 기반으로 문자열의 특정 부분을 검색하는 것에 대한 것입니다. 프로그래머들은 사용자 입력을 구문 분석하거나, 데이터를 조작하거나, 더 큰 텍스트 소스에서 관련 정보를 추출하는 등의 작업을 위해 이를 수행합니다.

## 어떻게:
Dart에서는 `substring()`, `split()`, 정규 표현식과 같은 다양한 메서드를 사용하여 부분 문자열을 추출할 수 있습니다. 각 메서드는 다양한 목적을 제공하며 문자열을 처리하는 유연성을 제공합니다.

### `substring()` 사용하기:
`substring()` 메서드는 간단합니다. 시작 인덱스(그리고 선택적으로 종료 인덱스)를 지정하여 문자열을 분할합니다.

```dart
void main() {
  String example = "Hello, World!";
  String result = example.substring(7, 12);
  print(result); // 출력: World
}
```

### `split()` 사용하기:
문자열을 패턴(공백이나 쉼표 같은)을 기준으로 부분 문자열 리스트로 분할하고, 그 다음 인덱스로 부분 문자열을 접근합니다.

```dart
void main() {
  String example = "Dart is fun";
  List<String> parts = example.split(' ');
  String result = parts[1]; // 인덱스로 접근
  print(result); // 출력: is
}
```

### 정규 표현식 사용하기:
복잡한 패턴의 경우, Dart의 `RegExp` 클래스는 강력합니다. 이를 사용하여 패턴을 매치하고 부분 문자열을 추출합니다.

```dart
void main() {
  String example = "Email: example@mail.com";
  RegExp regExp = RegExp(r"\b\w+@\w+\.\w+\b");
  String email = regExp.stringMatch(example)!;
  print(email); // 출력: example@mail.com
}
```

### 서드파티 라이브러리:
Dart의 표준 라이브러리는 상당히 유능하지만, 특정 작업을 단순화할 수 있는 서드파티 라이브러리를 만날 수 있는 상황이 있을 수 있습니다. 문자열 조작 및 패턴 매칭에 대해 특별히 지지하는 선호 라이브러리는 없지만 Dart의 내장 기능이 종종 충분하다고 여겨집니다. 그럼에도 특정 필요에 더 잘 맞는 라이브러리가 있는지 항상 [pub.dev](https://pub.dev)를 확인하세요.
