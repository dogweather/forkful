---
title:                "CSV와 함께 작업하기"
date:                  2024-03-08T21:57:22.659933-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?

CSV(콤마로 구분된 값) 파일을 다루는 작업은 각 줄이 콤마로 구분된 값을 가지고 있는 텍스트 파일을 파싱하고 생성하는 것을 포함합니다. 프로그래머들은 다른 애플리케이션 간의 데이터 교환을 가능하게 하거나, 가벼우면서도 human-readable한 형식으로 데이터를 저장하기 위해 이를 수행합니다.

## 어떻게 할까요:

Dart에서 CSV 파일을 다루기 위해, 일반적으로 텍스트를 수동으로 처리하거나 작업을 단순화하기 위해 제3자 라이브러리를 사용합니다. 여기서는 두 가지 접근법 모두를 살펴볼 것입니다.

### 수동으로 CSV 파싱하기

당신의 요구사항이 단순하다면, CSV 문자열을 수동으로 파싱하기로 선택할 수 있습니다. 이는 Dart의 핵심 문자열 조작 함수를 사용하여 달성할 수 있습니다:

```dart
void main() {
  // 샘플 CSV 데이터
  String csvData = "Name,Age,Email\nJohn Doe,30,john@example.com\nJane Smith,25,jane@example.com";
  
  // CSV 데이터를 줄로 분할하기
  List<String> lines = csvData.split('\n');
  
  // 각 줄 파싱하기
  List<Map<String, String>> data = [];
  List<String> headers = lines.first.split(',');
  
  for (var i = 1; i < lines.length; i++) {
    List<String> row = lines[i].split(',');
    Map<String, String> record = {};
    for (var j = 0; j < headers.length; j++) {
      record[headers[j]] = row[j];
    }
    data.add(record);
  }
  
  // 파싱된 데이터 출력하기
  print(data);
}

// 샘플 출력:
// [{Name: John Doe, Age: 30, Email: john@example.com}, {Name: Jane Smith, Age: 25, Email: jane@example.com}]
```

### 제3자 라이브러리 사용하기: `csv`

더 복잡한 시나리오를 위해 혹은 코드를 단순화하기 위해, `csv`와 같은 인기 있는 제3자 라이브러리를 사용할 수 있습니다. 먼저, 당신의 프로젝트에 추가하기 위해 `dependencies` 아래 `pubspec.yaml` 파일에 `csv: ^5.0.0` (또는 최신 버전)을 포함시키세요. 그런 다음 다음과 같이 사용하세요:

```dart
import 'package:csv/csv.dart';

void main() {
  String csvData = "Name,Age,Email\nJohn Doe,30,john@example.com\nJane Smith,25,jane@example.com";
  
  // CsvToListConverter를 사용하여 CSV 데이터 파싱하기
  List<List<dynamic>> listData = const CsvToListConverter().convert(csvData);
  
  // 첫 번째 리스트 항목은 헤더를 포함함
  List<String> headers = listData.first.map((item) => item.toString()).toList();
  
  // 처리를 계속하기 전에 헤더 행을 제거하기
  listData.removeAt(0);
  
  // 더 구조화된 형식으로 변환하기 위해 List<Map<String, dynamic>>로 변환하기
  List<Map<String, dynamic>> mappedData = listData.map((list) {
    Map<String, dynamic> map = {};
    for (int i = 0; i < headers.length; i++) {
      map[headers[i]] = list[i];
    }
    return map;
  }).toList();
  
  // 매핑된 데이터 출력하기
  print(mappedData);
}

// 샘플 출력:
// [{Name: John Doe, Age: 30, Email: john@example.com}, {Name: Jane Smith, Age: 25, Email: jane@example.com}]
```

두 가지 방법 모두 CSV 데이터를 다루는 방법을 보여줍니다: 첫 번째는 학습 목적이나 매우 단순한 CSV 구조를 다룰 때 수동으로; 두 번째는 파싱을 단순화하고 CSV 포매팅의 다양한 복잡성을 처리할 수 있는 강력한 라이브러리를 활용하여 진행합니다.
