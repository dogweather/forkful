---
title:                "HTTP 요청 보내기"
date:                  2024-03-08T21:57:07.934272-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?

Dart에서 HTTP 요청을 보내는 것은 Dart 애플리케이션에서 웹 서버 또는 API와 통신을 시작하는 과정입니다. 프로그래머들은 웹에서 데이터를 가져오거나, 폼을 제출하거나, RESTful 서비스와 상호 작용하기 위해 이 작업을 수행합니다. 이는 Dart의 웹, 서버 측, 모바일 애플리케이션 개발에 있어 기본적인 작업입니다.

## 방법:

Dart는 HTTP 리소스를 작업하는 데 강력하고 편리한 방법을 제공하는 `http` 패키지를 포함하고 있습니다. 먼저, 대상의 pubspec.yaml 파일에 이를 포함시키십시오:

```yaml
dependencies:
  http: ^0.13.3
```

그런 다음 Dart 코드에서 이를 import 하여 요청을 시작하십시오:

```dart
import 'package:http/http.dart' as http;

void main() async {
  var url = Uri.parse('https://jsonplaceholder.typicode.com/todos/1');
  var response = await http.get(url);

  if (response.statusCode == 200) {
    print('Response body: ${response.body}');
  } else {
    print('Request failed with status: ${response.statusCode}.');
  }
}
```

성공적인 요청에 대한 샘플 출력은 다음과 같을 수 있습니다:

```
Response body: {
  "userId": 1,
  "id": 1,
  "title": "delectus aut autem",
  "completed": false
}
```

JSON 본문이 있는 POST 요청과 같은 더 복잡한 요청의 경우 다음과 같이 하십시오:

```dart
import 'dart:convert';
import 'package:http/http.dart' as http;

void main() async {
  var url = Uri.parse('https://jsonplaceholder.typicode.com/posts');
  var response = await http.post(
    url,
    headers: {"Content-Type": "application/json"},
    body: jsonEncode({
      "title": 'foo',
      "body": 'bar',
      "userId": 1,
    }),
  );

  if (response.statusCode == 201) {
    print('Response status: ${response.statusCode}');
    print('Response body: ${response.body}');
  } else {
    print('Failed to create a new post. Status: ${response.statusCode}');
  }
}
```

POST 요청에 대한 샘플 출력은 다음과 같을 수 있습니다:

```
Response status: 201
Response body: {
  "title": "foo",
  "body": "bar",
  "userId": 1,
  "id": 101
}
```

이 예시들은 Dart의 `http` 패키지를 사용하여 기본적인 HTTP GET 및 POST 요청을 보여줍니다. 이 패키지는 헤더와 본문 콘텐츠가 포함된 더 복잡한 시나리오를 포함하여 HTTP 요청을 보내는 데 필요한 대부분의 필요사항을 다룹니다.
