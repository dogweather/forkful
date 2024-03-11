---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:17.843629-07:00
description: "Dart\uC5D0\uC11C \uD14D\uC2A4\uD2B8 \uD30C\uC77C\uC744 \uC77D\uB294\
  \ \uAC83\uC740 \uD30C\uC77C \uC2DC\uC2A4\uD15C\uC5D0 \uC800\uC7A5\uB41C \uD30C\uC77C\
  \uC5D0\uC11C \uB370\uC774\uD130\uC5D0 \uC811\uADFC\uD558\uACE0 \uAC80\uC0C9\uD558\
  \uB294 \uAC83\uC744 \uD3EC\uD568\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\
  \uB4E4\uC740 \uC774\uAC83\uC744 \uC785\uB825 \uB370\uC774\uD130, \uAD6C\uC131 \uC124\
  \uC815 \uB610\uB294 \uB370\uC774\uD130\uC14B\uC744 \uC77D\uAE30 \uC704\uD574 \uC0AC\
  \uC6A9\uD558\uBA70, \uAC04\uB2E8\uD55C \uC2A4\uD06C\uB9BD\uD2B8\uBD80\uD130 \uBCF5\
  \uC7A1\uD55C \uC571\uC5D0 \uC774\uB974\uAE30\uAE4C\uC9C0 \uB9CE\uC740 \uC560\uD50C\
  \uB9AC\uCF00\uC774\uC158\uC5D0 \uC788\uC5B4 \uAE30\uBCF8\uC801\uC778\u2026"
lastmod: '2024-03-11T00:14:28.735416-06:00'
model: gpt-4-0125-preview
summary: "Dart\uC5D0\uC11C \uD14D\uC2A4\uD2B8 \uD30C\uC77C\uC744 \uC77D\uB294 \uAC83\
  \uC740 \uD30C\uC77C \uC2DC\uC2A4\uD15C\uC5D0 \uC800\uC7A5\uB41C \uD30C\uC77C\uC5D0\
  \uC11C \uB370\uC774\uD130\uC5D0 \uC811\uADFC\uD558\uACE0 \uAC80\uC0C9\uD558\uB294\
  \ \uAC83\uC744 \uD3EC\uD568\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\
  \uC740 \uC774\uAC83\uC744 \uC785\uB825 \uB370\uC774\uD130, \uAD6C\uC131 \uC124\uC815\
  \ \uB610\uB294 \uB370\uC774\uD130\uC14B\uC744 \uC77D\uAE30 \uC704\uD574 \uC0AC\uC6A9\
  \uD558\uBA70, \uAC04\uB2E8\uD55C \uC2A4\uD06C\uB9BD\uD2B8\uBD80\uD130 \uBCF5\uC7A1\
  \uD55C \uC571\uC5D0 \uC774\uB974\uAE30\uAE4C\uC9C0 \uB9CE\uC740 \uC560\uD50C\uB9AC\
  \uCF00\uC774\uC158\uC5D0 \uC788\uC5B4 \uAE30\uBCF8\uC801\uC778\u2026"
title: "\uD14D\uC2A4\uD2B8 \uD30C\uC77C \uC77D\uAE30"
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
