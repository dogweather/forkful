---
title:                "Baixando uma página da web"
date:                  2024-03-08T21:54:42.431726-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## O Que & Por Que?

Baixar uma página web envolve buscar o conteúdo de uma página web através de sua URL para processamento ou armazenamento. Programadores fazem isso para extrair informações, monitorar mudanças, ou arquivar conteúdo, tornando-o um ponto básico em raspagem de web, mineração de dados e tarefas de teste automatizado.

## Como fazer:

Dart fornece o pacote `http`, uma biblioteca de terceiros popular para realizar requisições HTTP. Aqui está um exemplo básico de como usá-lo para baixar uma página web:

Primeiro, adicione o pacote `http` ao seu `pubspec.yaml`:

```yaml
dependencies:
  http: ^0.13.3
```

Depois, importe o pacote e use-o para buscar o conteúdo de uma página web:

```dart
import 'package:http/http.dart' as http;

Future<void> main() async {
  var url = Uri.parse('http://example.com');
  var resposta = await http.get(url);
  if (resposta.statusCode == 200) {
    print('Página baixada:');
    print(resposta.body);
  } else {
    print('Requisição falhou com status: ${resposta.statusCode}.');
  }
}
```

**Saída de exemplo** (isso variará com base no conteúdo da página web):

```
Página baixada:
<!doctype html>
<html>
<head>
    <title>Exemplo de Domínio</title>
...
</html>
```

Para cenários mais complexos, como lidar com cookies ou configurar cabeçalhos de user-agent, você usaria o mesmo pacote `http`, mas com configurações adicionais para a sua requisição:

```dart
import 'package:http/http.dart' as http;

Future<void> main() async {
  var cabeçalhos = {
    'User-Agent': 'SeuUserAgentCustomizado/1.0',
    'Cookie': 'nome=valor; nome2=valor2',
  };
  var url = Uri.parse('http://example.com');
  var resposta = await http.get(url, cabeçalhos: cabeçalhos);

  if (resposta.statusCode == 200) {
    print('Página baixada com cabeçalhos personalizados:');
    print(resposta.body);
  } else {
    print('Requisição falhou com status: ${resposta.statusCode}.');
  }
}
```

Usar cabeçalhos como estes pode imitar requisições de navegador mais precisamente, o que é particularmente útil ao lidar com sites que têm requisitos específicos ou proteções contra raspagem.
