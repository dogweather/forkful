---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:53:28.621433-07:00
description: "Como fazer: Dart usa a biblioteca `dart:io` para trabalhar com arquivos\
  \ e diret\xF3rios. Aqui est\xE1 uma maneira simples de verificar se um diret\xF3\
  rio existe."
lastmod: '2024-03-13T22:44:46.297250-06:00'
model: gpt-4-0125-preview
summary: "Dart usa a biblioteca `dart:io` para trabalhar com arquivos e diret\xF3\
  rios."
title: "Verificando se um diret\xF3rio existe"
weight: 20
---

## Como fazer:
Dart usa a biblioteca `dart:io` para trabalhar com arquivos e diretórios. Aqui está uma maneira simples de verificar se um diretório existe:

```dart
import 'dart:io';

void main() {
  var directory = Directory('caminho/para/seu/diretório');

  if (directory.existsSync()) {
    print('Diretório existe');
  } else {
    print('Diretório não existe');
  }
}
```
Saída de amostra se o diretório existir:
```
Diretório existe
```

Ou, se não:
```
Diretório não existe
```

Para lidar com cenários mais complexos, como verificar de forma assíncrona ou criar um diretório se ele não existir, você poderia usar a seguinte abordagem:

```dart
import 'dart:io';

void main() async {
  var directory = Directory('caminho/para/seu/diretório');

  // Verifica de forma assíncrona se o diretório existe
  var exists = await directory.exists();
  if (exists) {
    print('Diretório existe');
  } else {
    print('Diretório não existe, criando...');
    await directory.create(); // Isso cria o diretório
    print('Diretório criado');
  }
}
```

Saída de amostra se o diretório não existia e foi criado:
```
Diretório não existe, criando...
Diretório criado
```

As capacidades integradas do Dart geralmente são suficientes para lidar com arquivos e diretórios, então bibliotecas de terceiros normalmente não são necessárias para essa tarefa. No entanto, para operações mais complexas no sistema de arquivos, pacotes como `path` (para manipular caminhos de uma forma independente de plataforma) podem complementar a biblioteca `dart:io`, mas não oferecem diretamente verificações de existência de diretório mais avançadas do que o que foi mostrado.
