---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:23.510284-07:00
description: "Dart\uC5D0\uC11C \uBB38\uC790\uC5F4\uC5D0\uC11C \uB0A0\uC9DC\uB97C \uD30C\
  \uC2F1\uD558\uB294 \uAC83\uC740 \uB0A0\uC9DC\uC640 \uC2DC\uAC04\uC758 \uD14D\uC2A4\
  \uD2B8 \uD45C\uD604\uC744 `DateTime` \uAC1D\uCCB4\uB85C \uBCC0\uD658\uD558\uB294\
  \ \uACFC\uC815\uC744 \uD3EC\uD568\uD569\uB2C8\uB2E4. \uC774 \uC791\uC5C5\uC740 \uC2A4\
  \uCF00\uC904\uB9C1, \uB370\uC774\uD130 \uBD84\uC11D \uB610\uB294 \uB0A0\uC9DC \uC870\
  \uC791\uC774 \uD544\uC694\uD55C \uBAA8\uB4E0 \uAE30\uB2A5\uC744 \uB2E4\uB8E8\uB294\
  \ \uC751\uC6A9 \uD504\uB85C\uADF8\uB7A8\uC5D0 \uD544\uC218\uC801\uC774\uBA70, \uD504\
  \uB85C\uADF8\uB7A8\uC774 \uB0A0\uC9DC \uAD00\uB828 \uB370\uC774\uD130\uB97C \uC62C\
  \uBC14\uB974\uAC8C\u2026"
lastmod: '2024-03-11T00:14:28.722011-06:00'
model: gpt-4-0125-preview
summary: "Dart\uC5D0\uC11C \uBB38\uC790\uC5F4\uC5D0\uC11C \uB0A0\uC9DC\uB97C \uD30C\
  \uC2F1\uD558\uB294 \uAC83\uC740 \uB0A0\uC9DC\uC640 \uC2DC\uAC04\uC758 \uD14D\uC2A4\
  \uD2B8 \uD45C\uD604\uC744 `DateTime` \uAC1D\uCCB4\uB85C \uBCC0\uD658\uD558\uB294\
  \ \uACFC\uC815\uC744 \uD3EC\uD568\uD569\uB2C8\uB2E4. \uC774 \uC791\uC5C5\uC740 \uC2A4\
  \uCF00\uC904\uB9C1, \uB370\uC774\uD130 \uBD84\uC11D \uB610\uB294 \uB0A0\uC9DC \uC870\
  \uC791\uC774 \uD544\uC694\uD55C \uBAA8\uB4E0 \uAE30\uB2A5\uC744 \uB2E4\uB8E8\uB294\
  \ \uC751\uC6A9 \uD504\uB85C\uADF8\uB7A8\uC5D0 \uD544\uC218\uC801\uC774\uBA70, \uD504\
  \uB85C\uADF8\uB7A8\uC774 \uB0A0\uC9DC \uAD00\uB828 \uB370\uC774\uD130\uB97C \uC62C\
  \uBC14\uB974\uAC8C\u2026"
title: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB0A0\uC9DC \uBD84\uC11D\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇이며 왜?
Dart에서 문자열에서 날짜를 파싱하는 것은 날짜와 시간의 텍스트 표현을 `DateTime` 객체로 변환하는 과정을 포함합니다. 이 작업은 스케줄링, 데이터 분석 또는 날짜 조작이 필요한 모든 기능을 다루는 응용 프로그램에 필수적이며, 프로그램이 날짜 관련 데이터를 올바르게 이해하고 처리할 수 있도록 합니다.

## 방법:
Dart의 코어 라이브러리는 `DateTime` 클래스를 통해 날짜 파싱을 간소화합니다. 날짜 문자열의 형식을 알고 있는 간단한 경우에는 `DateTime.parse()` 메서드를 사용할 수 있습니다. 그러나 더 복잡한 시나리오나 다양한 형식을 다룰 때는 `intl` 패키지, 특히 `DateFormat` 클래스가 가치가 있습니다.

### Dart 코어 라이브러리 사용하기:
```dart
void main() {
  // DateTime.parse() 사용
  var dateString = "2023-10-31";
  var parsedDate = DateTime.parse(dateString);
  
  print(parsedDate); // 2023-10-31 00:00:00.000
}
```

### `intl` 패키지 사용하기:
먼저, `pubspec.yaml` 파일에 `intl` 패키지를 추가하세요:
```yaml
dependencies:
  intl: ^0.17.0
```
그 다음, 패키지를 import하고 파싱을 위해 `DateFormat`을 사용하세요:
```dart
import 'package:intl/intl.dart';

void main() {
  var dateString = "October 31, 2023";
  var dateFormat = DateFormat("MMMM dd, yyyy");
  var parsedDate = dateFormat.parse(dateString);
  
  print(parsedDate); // 2023-10-31 00:00:00.000
}
```
`intl` 패키지는 각종 국제 날짜 형식을 매끄럽게 처리할 수 있는 강력한 옵션을 제공합니다.
