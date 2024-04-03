---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:30.808628-07:00
description: "\uBC29\uBC95: Dart\uB294 `length` \uC18D\uC131\uC744 \uC0AC\uC6A9\uD558\
  \uC5EC \uBB38\uC790\uC5F4\uC758 \uAE38\uC774\uB97C \uC27D\uAC8C \uC5BB\uC744 \uC218\
  \ \uC788\uAC8C \uD574\uC90D\uB2C8\uB2E4. \uAE30\uBCF8 \uC608\uC2DC\uB294 \uB2E4\uC74C\
  \uACFC \uAC19\uC2B5\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:54.772587-06:00'
model: gpt-4-0125-preview
summary: "Dart\uB294 `length` \uC18D\uC131\uC744 \uC0AC\uC6A9\uD558\uC5EC \uBB38\uC790\
  \uC5F4\uC758 \uAE38\uC774\uB97C \uC27D\uAC8C \uC5BB\uC744 \uC218 \uC788\uAC8C \uD574\
  \uC90D\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4\uC758 \uAE38\uC774 \uCC3E\uAE30"
weight: 7
---

## 방법:
Dart는 `length` 속성을 사용하여 문자열의 길이를 쉽게 얻을 수 있게 해줍니다. 기본 예시는 다음과 같습니다:

```dart
void main() {
  String myString = "Hello, Dart!";
  print("'\(myString)'의 길이는: \(myString.length)");
  // 출력: 'Hello, Dart!'의 길이는: 12
}
```
이 속성은 문자열의 UTF-16 코드 단위 수를 계산하며, 이는 대부분의 일반적인 사용 사례에 해당하는 문자열의 길이와 일치합니다.

보다 섬세한 텍스트 처리, 특히 기본 다국어 평면(BMP) 밖의 유니코드 문자를 다룰 경우, 사용자가 인식하는 문자를 더 정확하게 대표하는 그래플 클러스터를 계산하기 위해 `characters` 패키지를 사용하는 것을 고려해보세요.

먼저, `characters`를 `pubspec.yaml`에 추가하세요:

```yaml
dependencies:
  characters: ^1.2.0
```

그런 다음, 다음과 같이 사용하세요:

```dart
import 'package:characters/characters.dart';

void main() {
  String myEmojiString = "👨‍👩‍👧‍👦 family";
  print("'\(myEmojiString)'의 길이는: \(myEmojiString.characters.length)");
  // 출력: '👨‍👩‍👧‍👦 family'의 길이는: 8
}
```

이 예시에서 `myEmojiString.characters.length`는 이모티콘 또는 결합 문자 표기처럼 복잡한 문자를 포함하는 문자열에 대해 더 정확한 표현인 유니코드 그래플 클러스터 측면에서의 길이를 제공합니다.
