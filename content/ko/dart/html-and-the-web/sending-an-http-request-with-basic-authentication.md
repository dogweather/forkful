---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:21.535559-07:00
description: "\uC5B4\uB5BB\uAC8C: Dart\uC5D0\uC11C\uB294 `http` \uD328\uD0A4\uC9C0\
  \uB97C \uC0AC\uC6A9\uD558\uC5EC \uAE30\uBCF8 \uC778\uC99D\uACFC \uD568\uAED8 HTTP\
  \ \uC694\uCCAD\uC744 \uBCF4\uB0BC \uC218 \uC788\uC2B5\uB2C8\uB2E4. \uBA3C\uC800\
  , `http` \uD328\uD0A4\uC9C0\uB97C `pubspec.yaml` \uD30C\uC77C\uC5D0 \uCD94\uAC00\
  \uD569\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:54.787818-06:00'
model: gpt-4-0125-preview
summary: "Dart\uC5D0\uC11C\uB294 `http` \uD328\uD0A4\uC9C0\uB97C \uC0AC\uC6A9\uD558\
  \uC5EC \uAE30\uBCF8 \uC778\uC99D\uACFC \uD568\uAED8 HTTP \uC694\uCCAD\uC744 \uBCF4\
  \uB0BC \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "\uAE30\uBCF8 \uC778\uC99D\uC744 \uC774\uC6A9\uD55C HTTP \uC694\uCCAD \uC804\
  \uC1A1\uD558\uAE30"
weight: 45
---

## 어떻게:
Dart에서는 `http` 패키지를 사용하여 기본 인증과 함께 HTTP 요청을 보낼 수 있습니다. 먼저, `http` 패키지를 `pubspec.yaml` 파일에 추가합니다:

```yaml
dependencies:
  http: ^0.13.4
```

그런 다음, Dart 파일에서 패키지를 가져옵니다:

```dart
import 'package:http/http.dart' as http;
import 'dart:convert';
```

기본 인증을 사용하여 GET 요청을 보내려면 다음 코드를 사용할 수 있습니다:

```dart
Future<void> fetchUserData() async {
  final username = 'yourUsername';
  final password = 'yourPassword';
  final credentials = base64Encode(utf8.encode('$username:$password'));
  final response = await http.get(
    Uri.parse('https://yourapi.com/userdata'),
    headers: {
      'Authorization': 'Basic $credentials',
    },
  );

  if (response.statusCode == 200) {
    print('사용자 데이터를 성공적으로 가져왔습니다!');
    print('응답 본문: ${response.body}');
  } else {
    print('상태 코드를 사용하여 사용자 데이터를 가져오지 못했습니다: ${response.statusCode}');
  }
}
```

이 코드는 기본 인증 헤더를 사용하여 'https://yourapi.com/userdata'로 GET 요청을 보냅니다. 사용자 이름과 비밀번호는 base64로 인코딩되어 기본 액세스 인증 표준에 따라 'Authorization' 헤더에 전달됩니다.

**샘플 출력:**

요청이 성공적이고 서버가 상태 코드 200을 반환하면 다음과 같이 볼 수 있습니다:

```plaintext
사용자 데이터를 성공적으로 가져왔습니다!
응답 본문: {"id":1, "name":"John Doe", "email":"john@example.com"}
```

인증에 실패하거나 다른 오류가 있는 경우, 응답 상태 코드를 통해 문제를 식별할 수 있습니다.
