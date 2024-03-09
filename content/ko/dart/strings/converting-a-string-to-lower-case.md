---
title:                "문자열을 소문자로 변환하기"
date:                  2024-03-08T21:54:07.338112-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## 무엇이며 왜?

문자열을 소문자로 변환하는 것은 주어진 문자열의 모든 문자를 소문자로 변환하는 기본적인 작업입니다. 프로그래머는 일반적으로 대소문자 구분 없는 비교를 달성하거나 처리를 위한 텍스트 입력을 표준화하기 위해 이 작업을 수행하여 애플리케이션을 더 사용자 친화적으로 만들고 데이터를 더 일관되게 만듭니다.

## 방법:

Dart에서는 `String` 클래스에서 제공하는 `toLowerCase()` 메소드를 사용하여 문자열을 소문자로 변환할 수 있습니다. 이 메소드는 모든 대문자를 소문자로 변환한 새 문자열을 반환합니다. 간단한 예제로 이 작업이 어떻게 이루어지는지 살펴봅시다:

```dart
void main() {
  String originalString = "Hello, World!";
  String lowerCaseString = originalString.toLowerCase();

  print(lowerCaseString);  // 출력: hello, world!
}
```

Dart는 소문자로 변환하는 것을 포함하여 기본 문자열 조작 작업을 위해 외부 라이브러리를 요구하지 않습니다. 표준 라이브러리의 `String` 클래스가 상당히 포괄적이기 때문입니다. 그러나 지역별 규칙을 포함한 더 복잡한 조작을 위해서는 국제화 및 지역화 기능을 제공하는 `intl` 패키지를 고려할 수 있습니다. 이에는 지역을 기반으로 한 대소문자 변환도 포함됩니다:

`intl`을 사용하려면 `pubspec.yaml` 파일에 추가하세요:

```yaml
dependencies:
  intl: ^0.17.0
```

그런 다음, 특정 지역에 기반하여 문자열을 소문자로 변환하기 위해 `toLocaleLowerCase()` 메소드를 사용할 수 있습니다:

```dart
import 'package:intl/intl.dart';

void main() {
  String originalString = "İstanbul";
  
  // 터키 지역
  print(Intl.withLocale('tr', () => originalString.toLowerCase())); // 출력: istanbul
  
  // 기본 지역 (en)
  print(originalString.toLowerCase()); // 출력: i̇stanbul
}
```

이 예제에서는 터키어 지역 설정이 점 없는 'i'를 올바르게 처리하는 방식을 주목하세요. 이는 국제화된 애플리케이션에서 지역별 변환의 중요성을 보여줍니다.
