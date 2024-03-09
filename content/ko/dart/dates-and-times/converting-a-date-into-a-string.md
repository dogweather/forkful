---
title:                "날짜를 문자열로 변환하기"
date:                  2024-03-08T21:53:59.112462-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## 무엇을, 왜?

Dart에서 날짜를 문자열로 변환하는 것은 날짜와 시간 정보를 사람이 읽을 수 있는 형식으로 표시하거나 데이터를 저장 또는 전송을 위해 직렬화하려는 경우에 흔히 필요한 작업입니다. 이 과정을 통해 날짜-시간 값의 표현과 조작이 이해하기 쉬우면서도 사용 사례에 따라 커스터마이즈될 수 있는 형식으로 쉽게 이루어질 수 있습니다.

## 방법:

Dart는 날짜와 시간을 다루기 위한 `DateTime` 클래스와 포맷팅을 위한 `intl` 패키지를 제공합니다. 먼저, `pubspec.yaml` 파일에 `intl: ^0.17.0` (또는 최신 버전)을 추가하여 `intl` 패키지가 있는지 확인하세요.

### Dart의 핵심 라이브러리 사용하기

```dart
DateTime now = DateTime.now();
String formattedDate = "${now.year}-${now.month}-${now.day}";
print(formattedDate); // 출력: 2023-4-12 (예를 들어, 현재 날짜에 따라 달라집니다)
```

이 예제는 `DateTime`의 속성에서 직접 문자열을 구성합니다.

### `intl` 패키지 사용하기

먼저, 패키지를 가져옵니다:

```dart
import 'package:intl/intl.dart';
```

그 다음, 날짜를 포맷합니다:

```dart
DateTime now = DateTime.now();
String formattedDate = DateFormat('yyyy-MM-dd').format(now);
print(formattedDate); // 출력: 2023-04-12
```

`intl` 패키지는 지역별 형식을 포함하여 훨씬 더 복잡한 포맷팅을 쉽게 할 수 있습니다:

```dart
String formattedDateLocale = DateFormat.yMMMMd('en_US').format(now);
print(formattedDateLocale); // 출력: April 12, 2023
```

이 예제들은 Dart의 핵심 기능을 사용하거나 보다 고급 포맷팅 옵션을 위해 `intl` 패키지를 활용하여 날짜를 문자열로 변환하고 포맷하는 간단하지만 강력한 방법을 보여줍니다.
