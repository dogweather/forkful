---
title:                "현재 날짜 가져오기"
date:                  2024-03-08T21:55:10.502442-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?
Dart에서 현재 날짜를 얻는 것은 시스템에 현재 날짜와 시간을 요청하는 것을 포함합니다. 이 기능은 이벤트의 타임스탬프, 사용자에게 현재 날짜를 표시하거나 기간을 계산하는 것과 같은 기능에서 일반적으로 사용됩니다. 효율적으로 현재 날짜를 검색하고 조작하는 방법을 아는 것은 스케줄링, 로깅, 시간에 민감한 기능에 기초적입니다.

## 방법:
Dart의 핵심 라이브러리는 `DateTime` 클래스를 통해 현재 날짜와 시간에 쉽게 접근할 수 있게 합니다. 다음은 현재 날짜를 얻기 위한 기본 예제입니다:

```dart
void main() {
  DateTime now = DateTime.now();
  print(now); // 예제 출력: 2023-04-12 10:00:00.000
}
```

만약 날짜 부분(년, 월, 일)만 필요하다면, `DateTime` 객체를 포맷할 수 있습니다:

```dart
void main() {
  DateTime now = DateTime.now();
  String formattedDate = "${now.year}-${now.month}-${now.day}";
  print(formattedDate); // 예제 출력: 2023-04-12
}
```

Dart는 더 복잡한 날짜 포매팅을 위한 내장 라이브러리를 포함하고 있지 않지만, 이 목적을 위해 `intl` 패키지를 사용할 수 있습니다. 첫 번째로, 패키지를 `pubspec.yaml`에 추가하세요:

```yaml
dependencies:
  intl: ^0.17.0
```

그러면 날짜를 쉽게 포맷할 수 있습니다:

```dart
import 'package:intl/intl.dart';

void main() {
  DateTime now = DateTime.now();
  String formattedDate = DateFormat('yyyy-MM-dd').format(now);
  print(formattedDate); // 예제 출력: 2023-04-12
}
```

더 고급 포매팅 옵션을 위해, `intl` 패키지에 의해 제공되는 `DateFormat` 클래스를 탐색하세요. 이 클래스는 다양한 패턴과 로케일을 지원합니다.
