---
title:                "텍스트 파일 읽기"
date:                  2024-03-08T21:56:17.843629-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?

Dart에서 텍스트 파일을 읽는 것은 파일 시스템에 저장된 파일에서 데이터에 접근하고 검색하는 것을 포함합니다. 프로그래머들은 이것을 입력 데이터, 구성 설정 또는 데이터셋을 읽기 위해 사용하며, 간단한 스크립트부터 복잡한 앱에 이르기까지 많은 애플리케이션에 있어 기본적인 작업입니다.

## 방법:

Dart의 핵심 라이브러리인 `dart:io`는 텍스트 파일을 동기적 또는 비동기적으로 읽을 수 있는 필요한 기능을 제공합니다. 다음은 두 가지 접근 방법입니다.

**동기적으로:**

```dart
import 'dart:io';

void main() {
  var fileName = "path/to/your/textfile.txt";
  var file = File(fileName);

  // 파일을 동기적으로 읽기
  var contents;
  try {
    contents = file.readAsStringSync();
    print(contents);
  } catch (e) {
    print('파일을 읽는 중 오류 발생: $e');
  }
}
```

**비동기적으로:**

특히 대용량 파일을 읽거나 반응성 있는 애플리케이션의 경우, 프로그램이 파일을 읽는 동안 차단되지 않도록 하려면:

```dart
import 'dart:io';

void main() async {
  var fileName = "path/to/your/textfile.txt";
  var file = File(fileName);

  try {
    String contents = await file.readAsString();
    print(contents);
  } catch (e) {
    print('파일을 읽는 중 오류 발생: $e');
  }
}
```

**샘플 출력:**

만약 당신의 텍스트 파일에 다음과 같은 내용이 포함되어 있다면:

```
Hello, Dart!
```

위의 두 방법 모두 다음을 출력할 것입니다:

```
Hello, Dart!
```

**서드-파티 라이브러리 사용하기:**

파일 작업을 간소화하거나 오류 처리를 향상시키는 것과 같은 추가 기능을 위해 `package:file`과 같은 서드-파티 라이브러리를 고려할 수 있습니다. 하지만 마지막 업데이트 시점에서 볼 때, 위에서 보여준 것처럼 핵심 `dart:io` 패키지를 직접 사용하는 것이 Dart에서 텍스트 파일을 읽는 가장 일반적이고 간단한 방법입니다.
