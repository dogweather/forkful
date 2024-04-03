---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:57.298937-07:00
description: "\uBC29\uBC95: Dart\uB294 \uAE30\uBCF8 \uB77C\uC774\uBE0C\uB7EC\uB9AC\
  \uC5D0\uC11C HTML \uD30C\uC2F1\uC744 \uC9C0\uC6D0\uD558\uC9C0 \uC54A\uC2B5\uB2C8\
  \uB2E4. \uADF8\uB7EC\uB098 `html`\uACFC \uAC19\uC740 \uC11C\uB4DC\uD30C\uD2F0 \uD328\
  \uD0A4\uC9C0\uB97C \uC0AC\uC6A9\uD558\uC5EC HTML \uBB38\uC11C\uB97C \uD30C\uC2F1\
  \uD558\uACE0 \uC870\uC791\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4. \uBA3C\uC800, `pubspec.yaml`\
  \ \uD30C\uC77C\uC5D0 `html` \uD328\uD0A4\uC9C0\uB97C \uCD94\uAC00\uD558\uC138\uC694\
  ."
lastmod: '2024-03-13T22:44:54.784236-06:00'
model: gpt-4-0125-preview
summary: "Dart\uB294 \uAE30\uBCF8 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uC5D0\uC11C HTML\
  \ \uD30C\uC2F1\uC744 \uC9C0\uC6D0\uD558\uC9C0 \uC54A\uC2B5\uB2C8\uB2E4."
title: "HTML \uD30C\uC2F1"
weight: 43
---

## 방법:
Dart는 기본 라이브러리에서 HTML 파싱을 지원하지 않습니다. 그러나 `html`과 같은 서드파티 패키지를 사용하여 HTML 문서를 파싱하고 조작할 수 있습니다.

먼저, `pubspec.yaml` 파일에 `html` 패키지를 추가하세요:

```yaml
dependencies:
  html: ^0.15.0
```

그런 다음, 패키지를 당신의 Dart 파일에 import하세요:

```dart
import 'package:html/parser.dart' show parse;
import 'package:html/dom.dart';
```

HTML을 포함한 문자열을 파싱하고 데이터를 추출하는 기본 예제는 다음과 같습니다:

```dart
void main() {
  var htmlDocument = """
  <html>
    <body>
      <h1>안녕, Dart!</h1>
      <p>이것은 샘플 HTML의 문단입니다</p>
    </body>
  </html>
  """;

  // HTML 문자열 파싱
  Document document = parse(htmlDocument);

  // 데이터 추출
  String title = document.querySelector('h1')?.text ?? "제목을 찾을 수 없습니다";
  String paragraph = document.querySelector('p')?.text ?? "문단을 찾을 수 없습니다";

  print('제목: $title');
  print('문단: $paragraph');
}
```

출력:

```
제목: 안녕, Dart!
문단: 이것은 샘플 HTML의 문단입니다
```

실제 웹 페이지와 상호 작용하기 위해, `html` 파싱을 HTTP 요청(`http` 패키지를 사용하여 웹 콘텐츠를 가져옴)과 결합할 수 있습니다. 빠른 예시가 여기 있습니다:

먼저, `html`과 함께 `http` 패키지를 추가하세요:

```yaml
dependencies:
  html: ^0.15.0
  http: ^0.13.3
```

그런 다음, 웹에서 HTML 페이지를 가져와서 파싱하세요:

```dart
import 'package:http/http.dart' as http;
import 'package:html/parser.dart' show parse;

void main() async {
  var url = 'https://example.com';
  
  // 웹 페이지를 가져옴
  var response = await http.get(Uri.parse(url));
  
  if (response.statusCode == 200) {
    var document = parse(response.body);

    // 페이지에 관심이 있는 <h1> 태그가 있다고 가정
    var headlines = document.querySelectorAll('h1').map((e) => e.text).toList();
    
    print('헤드라인: $headlines');
  } else {
    print('요청 실패, 상태: ${response.statusCode}.');
  }
}
```

주의: 위에 보여진 웹 스크레이핑 기법은 웹사이트의 이용 약관을 준수하며 책임감 있게 사용되어야 합니다.
