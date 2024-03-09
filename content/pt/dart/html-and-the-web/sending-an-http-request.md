---
title:                "Enviando uma solicitação HTTP"
date:                  2024-03-08T21:56:17.063633-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## O quê & Por quê?

Enviar uma solicitação HTTP em Dart é o processo de iniciar comunicações com um servidor web ou API a partir de uma aplicação Dart. Programadores fazem isso para buscar dados da web, submeter formulários e interagir com serviços RESTful, tornando-o uma operação fundamental para o desenvolvimento de aplicações web, do lado do servidor e móveis em Dart.

## Como fazer:

Dart inclui o pacote `http`, uma maneira poderosa e conveniente de trabalhar com recursos HTTP. Primeiro, inclua-o em seu arquivo pubspec.yaml:

```yaml
dependencies:
  http: ^0.13.3
```

Em seguida, importe-o no seu código Dart para começar a fazer solicitações:

```dart
import 'package:http/http.dart' as http;

void main() async {
  var url = Uri.parse('https://jsonplaceholder.typicode.com/todos/1');
  var response = await http.get(url);

  if (response.statusCode == 200) {
    print('Corpo da resposta: ${response.body}');
  } else {
    print('Falha na solicitação com status: ${response.statusCode}.');
  }
}
```

Um exemplo de saída para uma solicitação bem-sucedida poderia ser assim:

```
Corpo da resposta: {
  "userId": 1,
  "id": 1,
  "title": "delectus aut autem",
  "completed": false
}
```

Para solicitações mais complexas, como solicitações POST com um corpo JSON, você faria o seguinte:

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
    print('Status da resposta: ${response.statusCode}');
    print('Corpo da resposta: ${response.body}');
  } else {
    print('Falha ao criar um novo post. Status: ${response.statusCode}');
  }
}
```

Um exemplo de saída para a solicitação POST poderia ser:

```
Status da resposta: 201
Corpo da resposta: {
  "title": "foo",
  "body": "bar",
  "userId": 1,
  "id": 101
}
```

Estes exemplos mostram solicitações HTTP GET e POST básicas usando o pacote `http` em Dart. Este pacote cobre a maioria das necessidades para enviar solicitações HTTP, incluindo cenários mais complexos com cabeçalhos e conteúdo do corpo.
