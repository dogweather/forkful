---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:53:31.462000-07:00
description: "Como fazer: O Dart oferece v\xE1rias maneiras simples de concatenar\
  \ strings. Abaixo est\xE3o os m\xE9todos mais comuns: O operador `+` \xE9 a maneira\
  \ mais intuitiva\u2026"
lastmod: '2024-03-13T22:44:46.272813-06:00'
model: gpt-4-0125-preview
summary: "O Dart oferece v\xE1rias maneiras simples de concatenar strings."
title: Concatenando strings
weight: 3
---

## Como fazer:
O Dart oferece várias maneiras simples de concatenar strings. Abaixo estão os métodos mais comuns:

### Usando o Operador `+`
O operador `+` é a maneira mais intuitiva de juntar strings.
```dart
String cumprimento = 'Olá, ' + 'Mundo!';
print(cumprimento); // Saída: Olá, Mundo!
```

### Usando o Método `concat()`
Embora o Dart não tenha um método `concat()` semelhante a outras linguagens, alcançar o mesmo pode ser feito usando `+` ou os métodos a seguir.

### Usando Interpolação de Strings
A interpolação de strings permite que variáveis sejam embutidas diretamente dentro de uma string. É eficiente para combinar strings e expressões.
```dart
String usuario = 'Jane';
String mensagem = 'Bem-vindo(a), $usuario!';
print(mensagem); // Saída: Bem-vindo(a), Jane!
```

### Usando o Método `join()`
O método `join()` é útil quando você tem uma lista de strings que deseja concatenar.
```dart
var palavras = ['Olá', 'do', 'Dart'];
String frase = palavras.join(' '); // Juntar com um separador de espaço.
print(frase); // Saída: Olá do Dart
```

### Usando StringBuffer
`StringBuffer` é eficiente para múltiplas concatenações, especialmente em loops.
```dart
var palavras = ['Dart', 'é', 'divertido'];
StringBuffer buffer = StringBuffer();
for (String palavra in palavras) {
  buffer.write(palavra); // Anexar cada palavra ao buffer.
  buffer.write(' '); // Opcionalmente adicionar um espaço.
}
String frase = buffer.toString().trim(); // Converter para string e remover espaço final.
print(frase); // Saída: Dart é divertido
```

### Bibliotecas de Terceiros
Embora a biblioteca padrão do Dart seja geralmente suficiente para tarefas de concatenação de strings, bibliotecas de terceiros como `quiver` oferecem utilitários que podem complementar a funcionalidade integrada do Dart. Por exemplo, as funções `concat()` ou `merge()` do `quiver` podem ser exploradas para cenários avançados. No entanto, apegue-se às robustas opções integradas do Dart, a menos que você tenha uma necessidade específica que elas não cobrem.
