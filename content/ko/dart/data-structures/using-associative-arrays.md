---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:02.116868-07:00
description: "\uBC29\uBC95: Dart\uB294 \uB9F5\uC744 \uC0DD\uC131\uD558\uACE0 \uC870\
  \uC791\uD558\uAE30 \uC704\uD55C \uC9C1\uAD00\uC801\uC778 \uBB38\uBC95\uC744 \uC81C\
  \uACF5\uD569\uB2C8\uB2E4. \uC544\uB798 \uC608\uC81C\uC5D0\uB294 \uC0DD\uC131, \uC694\
  \uC18C \uCD94\uAC00, \uAC12 \uAC00\uC838\uC624\uAE30\uC640 \uAC19\uC740 \uAE30\uBCF8\
  \ \uC791\uC5C5\uC744 \uBCF4\uC5EC\uC8FC\uB294 \uC608\uAC00 \uB098\uC640 \uC788\uC2B5\
  \uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:54.775906-06:00'
model: gpt-4-0125-preview
summary: "Dart\uB294 \uB9F5\uC744 \uC0DD\uC131\uD558\uACE0 \uC870\uC791\uD558\uAE30\
  \ \uC704\uD55C \uC9C1\uAD00\uC801\uC778 \uBB38\uBC95\uC744 \uC81C\uACF5\uD569\uB2C8\
  \uB2E4."
title: "\uC5F0\uAD00 \uBC30\uC5F4 \uC0AC\uC6A9\uD558\uAE30"
weight: 15
---

## 방법:
Dart는 맵을 생성하고 조작하기 위한 직관적인 문법을 제공합니다. 아래 예제에는 생성, 요소 추가, 값 가져오기와 같은 기본 작업을 보여주는 예가 나와 있습니다.

```dart
void main() {
  // 맵 생성
  var fruitColors = {
    'apple': 'red',
    'banana': 'yellow',
    'grape': 'purple'
  };

  // 새로운 키-값 쌍 추가
  fruitColors['orange'] = 'orange';

  // 키로 값을 접근
  print(fruitColors['apple']); // 출력: red

  // 값을 업데이트
  fruitColors['banana'] = 'green';

  // 맵을 반복 처리
  fruitColors.forEach((fruit, color) {
    print('$fruit: $color');
  });
  // 샘플 출력:
  // apple: red
  // banana: green
  // grape: purple
  // orange: orange
}
```

복잡한 데이터 구조나 확장된 기능을 위해, Dart 프로그래머들은 종종 추가 라이브러리에 의존합니다. 그 중 하나가 `collection`인데, 이는 고급 컬렉션 유형과 유틸리티를 제공합니다. `collection`은 기본적인 맵의 처리 방식을 변경하지는 않지만, 유틸리티 함수와 더 정교한 컬렉션 유형으로 풍부하게 합니다. 여기에 그것을 값으로 맵을 정렬하는 데 사용하는 방법이 나와 있습니다:

먼저, `collection` 패키지가 `pubspec.yaml` 파일에 포함되어 있는지 확인하세요:

```yaml
dependencies:
  collection: ^1.15.0
```

그런 다음, 다음과 같이 사용할 수 있습니다:

```dart
import 'package:collection/collection.dart';

void main() {
  var fruitColors = {
    'apple': 'red',
    'banana': 'yellow',
    'grape': 'purple',
    'orange': 'orange'
  };

  // 값(색깔)으로 맵을 정렬
  var sortedFruitsByColor = SplayTreeMap.from(
    fruitColors,
    (key1, key2) => fruitColors[key1]!.compareTo(fruitColors[key2]!)
  );

  print(sortedFruitsByColor);
  // 출력:
  // {orange: orange, apple: red, banana: yellow, grape: purple}
}
```

이 예제는 값에 기반하여 맵의 항목을 정렬하는 것을 보여줍니다. Dart와 그 활발한 생태계가 연관 배열을 더 정교한 데이터 조작을 위해 민첩하게 다룰 수 있음을 보여줍니다.
