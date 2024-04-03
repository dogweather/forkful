---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:07.934272-07:00
description: "\uBC29\uBC95: Dart\uB294 HTTP \uB9AC\uC18C\uC2A4\uB97C \uC791\uC5C5\uD558\
  \uB294 \uB370 \uAC15\uB825\uD558\uACE0 \uD3B8\uB9AC\uD55C \uBC29\uBC95\uC744 \uC81C\
  \uACF5\uD558\uB294 `http` \uD328\uD0A4\uC9C0\uB97C \uD3EC\uD568\uD558\uACE0 \uC788\
  \uC2B5\uB2C8\uB2E4. \uBA3C\uC800, \uB300\uC0C1\uC758 pubspec.yaml \uD30C\uC77C\uC5D0\
  \ \uC774\uB97C \uD3EC\uD568\uC2DC\uD0A4\uC2ED\uC2DC\uC624."
lastmod: '2024-03-13T22:44:54.782730-06:00'
model: gpt-4-0125-preview
summary: "Dart\uB294 HTTP \uB9AC\uC18C\uC2A4\uB97C \uC791\uC5C5\uD558\uB294 \uB370\
  \ \uAC15\uB825\uD558\uACE0 \uD3B8\uB9AC\uD55C \uBC29\uBC95\uC744 \uC81C\uACF5\uD558\
  \uB294 `http` \uD328\uD0A4\uC9C0\uB97C \uD3EC\uD568\uD558\uACE0 \uC788\uC2B5\uB2C8\
  \uB2E4."
title: "HTTP \uC694\uCCAD \uBCF4\uB0B4\uAE30"
weight: 44
---

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
