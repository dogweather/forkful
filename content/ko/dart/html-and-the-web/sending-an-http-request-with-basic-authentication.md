---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:21.535559-07:00
description: "\uAE30\uBCF8 \uC778\uC99D\uC744 \uC0AC\uC6A9\uD558\uC5EC HTTP \uC694\
  \uCCAD\uC744 \uBCF4\uB0B4\uB294 \uAC83\uC740 \uC0AC\uC6A9\uC790\uC758 \uC2E0\uC6D0\
  \uC744 \uD655\uC778\uD558\uAE30 \uC704\uD574 \uC694\uCCAD\uC5D0 \uC0AC\uC6A9\uC790\
  \ \uC774\uB984\uACFC \uBE44\uBC00\uBC88\uD638\uB97C \uCCA8\uBD80\uD558\uB294 \uAC83\
  \uC744 \uD3EC\uD568\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740\
  \ \uC778\uC99D\uC774 \uD544\uC694\uD55C \uB9AC\uC18C\uC2A4\uC5D0 \uC811\uADFC\uD558\
  \uAE30 \uC704\uD574 \uC774\uB97C \uC0AC\uC6A9\uD558\uC5EC \uD074\uB77C\uC774\uC5B8\
  \uD2B8\uC640 \uC11C\uBC84 \uAC04\uC758 \uC548\uC804\uD55C \uD1B5\uC2E0\uC744 \uBCF4\
  \uC7A5\uD569\uB2C8\uB2E4."
lastmod: '2024-03-11T00:14:28.705452-06:00'
model: gpt-4-0125-preview
summary: "\uAE30\uBCF8 \uC778\uC99D\uC744 \uC0AC\uC6A9\uD558\uC5EC HTTP \uC694\uCCAD\
  \uC744 \uBCF4\uB0B4\uB294 \uAC83\uC740 \uC0AC\uC6A9\uC790\uC758 \uC2E0\uC6D0\uC744\
  \ \uD655\uC778\uD558\uAE30 \uC704\uD574 \uC694\uCCAD\uC5D0 \uC0AC\uC6A9\uC790 \uC774\
  \uB984\uACFC \uBE44\uBC00\uBC88\uD638\uB97C \uCCA8\uBD80\uD558\uB294 \uAC83\uC744\
  \ \uD3EC\uD568\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC778\
  \uC99D\uC774 \uD544\uC694\uD55C \uB9AC\uC18C\uC2A4\uC5D0 \uC811\uADFC\uD558\uAE30\
  \ \uC704\uD574 \uC774\uB97C \uC0AC\uC6A9\uD558\uC5EC \uD074\uB77C\uC774\uC5B8\uD2B8\
  \uC640 \uC11C\uBC84 \uAC04\uC758 \uC548\uC804\uD55C \uD1B5\uC2E0\uC744 \uBCF4\uC7A5\
  \uD569\uB2C8\uB2E4."
title: "\uAE30\uBCF8 \uC778\uC99D\uC744 \uC774\uC6A9\uD55C HTTP \uC694\uCCAD \uC804\
  \uC1A1\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?

기본 인증을 사용하여 HTTP 요청을 보내는 것은 사용자의 신원을 확인하기 위해 요청에 사용자 이름과 비밀번호를 첨부하는 것을 포함합니다. 프로그래머들은 인증이 필요한 리소스에 접근하기 위해 이를 사용하여 클라이언트와 서버 간의 안전한 통신을 보장합니다.

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
