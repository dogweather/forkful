---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:15.598822-07:00
description: "Como fazer: Dart facilita a remo\xE7\xE3o de caracteres que correspondam\
  \ a um padr\xE3o pr\xE9-definido usando express\xF5es regulares e o m\xE9todo `replaceAll`.\
  \ N\xE3o s\xE3o\u2026"
lastmod: '2024-03-13T22:44:46.264256-06:00'
model: gpt-4-0125-preview
summary: "Dart facilita a remo\xE7\xE3o de caracteres que correspondam a um padr\xE3\
  o pr\xE9-definido usando express\xF5es regulares e o m\xE9todo `replaceAll`."
title: "Excluindo caracteres que correspondem a um padr\xE3o"
weight: 5
---

## Como fazer:
Dart facilita a remoção de caracteres que correspondam a um padrão pré-definido usando expressões regulares e o método `replaceAll`. Não são necessárias bibliotecas de terceiros para o uso básico, tornando esta abordagem muito acessível.

Aqui está um exemplo simples que demonstra como remover dígitos de uma string:

```dart
void main() {
  String stringWithDigits = 'Dart123 é divertido456';
  // Defina um padrão de expressão regular que corresponda a todos os dígitos
  RegExp digitPattern = RegExp(r'\d');
  
  // Substitua todas as ocorrências do padrão por uma string vazia
  String result = stringWithDigits.replaceAll(digitPattern, '');
  
  print(result); // Saída: Dart é divertido
}
```

Suponha que você esteja lidando com um cenário mais complexo, como remover caracteres especiais exceto espaços e pontuações. Eis como você faria:

```dart
void main() {
  String messyString = 'Dart!@# é *&()divertido$%^';
  // Define um padrão que corresponde a tudo, exceto letras, números, espaços e pontuações
  RegExp specialCharPattern = RegExp(r'[^a-zA-Z0-9 \.,!?]');
  
  String cleanedString = messyString.replaceAll(specialCharPattern, '');
  
  print(cleanedString); // Saída: Dart! é divertido
}
```

Para tarefas que requerem correspondência de padrões e substituição mais avançadas, a documentação abrangente da classe `RegExp` do Dart oferece um mergulho profundo em expressões mais complexas e seu uso. No entanto, os exemplos acima cobrem a maioria dos casos de uso comuns para a exclusão de caracteres baseados em padrões na programação Dart.
