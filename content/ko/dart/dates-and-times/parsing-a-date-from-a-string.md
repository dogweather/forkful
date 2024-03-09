---
title:                "문자열에서 날짜 분석하기"
date:                  2024-03-08T21:55:23.510284-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
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
