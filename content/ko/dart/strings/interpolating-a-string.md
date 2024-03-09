---
title:                "문자열 보간하기"
date:                  2024-03-08T21:54:57.863664-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?

문자열 보간은 변수 값들을 문자열 안에 직접 삽입하는 과정으로, 번거로운 연결 없이 의미 있는 메시지를 생성하기 위해 자주 사용됩니다. 프로그래머들은 더 깔끔하고 읽기 쉬운 코드를 위해, 그리고 복잡한 문자열 연결에서 발생하기 쉬운 오류들을 방지하기 위해 이를 실행합니다.

## 방법:

Dart에서 문자열 보간은 `$` 기호를 사용하여 문자열 리터럴 내에서 직접 표현식을 보간하는 것이 간단합니다:

```dart
void main() {
  String name = 'Dart';
  int year = 2023;
  // 간단한 변수 보간
  print('Learning $name in $year!');
  // 출력: Learning Dart in 2023!
  
  // 표현식 보간
  print('In two years, it will be ${year + 2}.');
  // 출력: In two years, it will be 2025.
}
```

더 복잡한 표현식이 있거나 문자열 자체 내에서 연산을 수행하고 싶은 경우, 표현식을 `${}`로 감싸세요. Dart는 다양하고 복잡한 시나리오를 원활하게 처리할 수 있도록 기본적으로 잘 갖추어져 있어, 문자열 보간을 위한 특별히 인기 있는 서드파티 라이브러리를 가지고 있지 않습니다.
