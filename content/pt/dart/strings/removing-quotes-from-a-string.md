---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:21.037190-07:00
description: "Remover aspas de uma string em Dart envolve retirar as aspas duplas\
  \ (\") ou simples (') do in\xEDcio e do fim de uma string, \xFAtil para limpeza\
  \ de dados ou\u2026"
lastmod: '2024-03-13T22:44:46.268535-06:00'
model: gpt-4-0125-preview
summary: "Remover aspas de uma string em Dart envolve retirar as aspas duplas (\"\
  ) ou simples (') do in\xEDcio e do fim de uma string, \xFAtil para limpeza de dados\
  \ ou prepara\xE7\xE3o de strings para processamento adicional."
title: Removendo aspas de uma string
weight: 9
---

## Como fazer:
O Dart oferece maneiras diretas de remover aspas de uma string usando métodos embutidos de string sem a necessidade de bibliotecas de terceiros.

### Exemplo 1: Usando `replaceFirst` e `replaceAll`
Se você está lidando com strings que começam e terminam com aspas, você pode usar os métodos `replaceFirst` e `replaceAll` para removê-las.

```dart
String quotedString = '"Olá, Mundo!"';
String singleQuotedString = '\'Programação Dart\'';

// Removendo aspas duplas
String noDoubleQuotes = quotedString.replaceFirst('"', '').replaceAll('"', '');
print(noDoubleQuotes); // Saída: Olá, Mundo!

// Removendo aspas simples
String noSingleQuotes = singleQuotedString.replaceFirst('\'', '').replaceAll('\'', '');
print(noSingleQuotes); // Saída: Programação Dart
```

### Exemplo 2: Usando `substring`
Este método é útil quando você tem certeza de que as aspas estão exatamente no início e no fim da string.

```dart
String quotedString = '"Desenvolvimento Flutter"';
// Verifique se começa e termina com aspas antes de remover para evitar erros
if (quotedString.startsWith('"') && quotedString.endsWith('"')) {
  quotedString = quotedString.substring(1, quotedString.length - 1);
}
print(quotedString); // Saída: Desenvolvimento Flutter
```

### Exemplo 3: Método de Extensão Personalizado
Para maior reusabilidade, particularmente se seu projeto envolve remoção frequente de aspas, considere criar uma extensão personalizada em `String`.

```dart
extension UnquoteString on String {
  String unquote() {
    var str = this;
    if (str.startsWith('"') && str.endsWith('"') || str.startsWith('\'') && str.endsWith('\'')) {
      str = str.substring(1, str.length - 1);
    }
    return str;
  }
}

void main() {
  String doubleQuoted = '"Isto é Dart"';
  String singleQuoted = '\'Isto é incrível\'';
  print(doubleQuoted.unquote()); // Saída: Isto é Dart
  print(singleQuoted.unquote()); // Saída: Isto é incrível
}
```

Estas abordagens devem ajudá-lo a remover aspas de strings eficazmente em Dart, aprimorando seus fluxos de trabalho de processamento e preparação de dados.
