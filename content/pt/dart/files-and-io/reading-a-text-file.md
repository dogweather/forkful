---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:31.102471-07:00
description: "Ler um arquivo de texto em Dart envolve acessar e recuperar dados de\
  \ arquivos armazenados no sistema de arquivos. Os programadores fazem isso para\u2026"
lastmod: '2024-03-13T22:44:46.300406-06:00'
model: gpt-4-0125-preview
summary: "Ler um arquivo de texto em Dart envolve acessar e recuperar dados de arquivos\
  \ armazenados no sistema de arquivos. Os programadores fazem isso para\u2026"
title: Lendo um arquivo de texto
---

{{< edit_this_page >}}

## O Que & Por Quê?

Ler um arquivo de texto em Dart envolve acessar e recuperar dados de arquivos armazenados no sistema de arquivos. Os programadores fazem isso para manipular dados de entrada, configurações ou ler conjuntos de dados, tornando-se uma operação fundamental para muitas aplicações, variando de scripts simples a aplicativos complexos.

## Como Fazer:

A biblioteca central do Dart, `dart:io`, fornece as funcionalidades necessárias para ler arquivos de texto de forma síncrona ou assíncrona. Veja como abordar ambos.

**De forma síncrona:**

```dart
import 'dart:io';

void main() {
  var fileName = "caminho/para/seu/arquivo.txt";
  var file = File(fileName);

  // Lendo o arquivo de forma síncrona
  var contents;
  try {
    contents = file.readAsStringSync();
    print(contents);
  } catch (e) {
    print('Erro ao ler arquivo: $e');
  }
}
```

**De forma assíncrona:**

Para evitar bloquear o programa enquanto o arquivo está sendo lido, especialmente útil para arquivos grandes ou aplicações responsivas:

```dart
import 'dart:io';

void main() async {
  var fileName = "caminho/para/seu/arquivo.txt";
  var file = File(fileName);

  try {
    String contents = await file.readAsString();
    print(contents);
  } catch (e) {
    print('Erro ao ler arquivo: $e');
  }
}
```

**Saída de Exemplo:**

Se o seu arquivo de texto contém:

```
Olá, Dart!
```

Ambos os métodos acima produzirão:

```
Olá, Dart!
```

**Usando uma Biblioteca de Terceiros:**

Para recursos adicionais, como operações de arquivo simplificadas ou tratamento de erros aprimorado, você pode considerar bibliotecas de terceiros, como `package:file`. No entanto, conforme minha última atualização, usar o pacote central `dart:io` diretamente, conforme mostrado acima, é o método mais comum e direto para ler arquivos de texto em Dart.
