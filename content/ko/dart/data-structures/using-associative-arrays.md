---
title:                "연관 배열 사용하기"
date:                  2024-03-08T21:57:02.116868-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?

Dart에서 연관 배열, 일반적으로 맵(Maps)으로 알려져 있는 것은 데이터를 키-값 쌍으로 저장하는 자료구조입니다. 프로그래머가 요소에 인덱스가 아닌 키를 통해 접근할 수 있게 함으로써, 각 요소가 고유한 식별자를 가진 구조화된 데이터를 다룰 때 직관적이고 효율적인 데이터 검색을 가능하게 합니다.

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
