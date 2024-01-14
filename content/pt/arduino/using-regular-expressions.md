---
title:    "Arduino: Usando expressões regulares"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

# Por que usar expressões regulares em Arduino?

Expressões regulares são uma ferramenta poderosa e versátil para manipulação de texto em qualquer linguagem de programação, incluindo Arduino. Elas permitem que você procure e substitua padrões em uma string de uma maneira muito eficiente e flexível. Se você está trabalhando com strings em seus projetos Arduino, deve considerar aprender a usar expressões regulares para economizar tempo e escrita de código repetitivo.

## Como usar expressões regulares em Arduino

Para começar a usar expressões regulares em seu código Arduino, você precisará importar a biblioteca "Regex". Você pode fazer isso abrindo a biblioteca na IDE do Arduino e selecionando "Incluir biblioteca" e depois "Regex". Em seguida, você pode seguir as instruções do exemplo fornecido para aprender como usar as funções básicas.

```Arduino
#include <Regex.h>

void setup() {
  Serial.begin(9600);
  // criar uma string para usar como exemplo
  String texto = "Olá, mundo! Esta é uma string de exemplo.";
  // criar um objeto regex para procurar pelo padrão "string"
  Regex regex("string");

  // usar a função .match() para procurar pelo padrão
  if (regex.match(texto)) {
    // se o padrão for encontrado, imprimir a posição e o tamanho da string
    Serial.println("O padrão foi encontrado em: " + regex.index() + ", com o tamanho de: " + regex.length());
  } else {
    Serial.println("O padrão não foi encontrado.");
  }
}

void loop() {
  // não é necessário fazer nada aqui para este exemplo
}
```

O código acima imprimirá "O padrão foi encontrado em: 24, com o tamanho de: 6" porque o padrão "string" começa na posição 24 da string "texto" e tem um tamanho de 6 caracteres. Você pode usar outras funções da biblioteca Regex para executar substituições, pesquisa global e muito mais.

## Mergulho profundo: Usando classes char e regex

Embora a biblioteca "Regex" seja conveniente para usar e entender, é importante saber que ela usa classes char e regex subjacentes para fazer todo o trabalho pesado. Você pode aprender mais sobre como essas classes funcionam na documentação da IDE do Arduino para se tornar um especialista em expressões regulares em Arduino.

# Veja também

- [Documentação da biblioteca Regex para Arduino](https://arduino.github.io/arduino-cli/latest/libraries/)
- [Tutorial de expressões regulares em Arduino](https://create.arduino.cc/projecthub/robekick/how-to-use-regex-regular-expressions-in-arduino-ide-tutorial-1-glyph-28d73b)