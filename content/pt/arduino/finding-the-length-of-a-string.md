---
title:                "Arduino: Encontrando o comprimento de uma string"
simple_title:         "Encontrando o comprimento de uma string"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por que alguém deve aprender a encontrar o comprimento de uma string
A programação em Arduino é uma ótima maneira de começar a se aventurar no mundo do desenvolvimento. Uma das habilidades mais úteis para um programador é ser capaz de encontrar o comprimento de uma string. Isso é importante para tarefas como manipulação de texto, armazenamento de dados e muito mais.

## Como encontrar o comprimento de uma string em Arduino
Encontrar o comprimento de uma string em Arduino pode parecer intimidante no início, mas é na verdade bastante simples. Vamos começar criando uma string e salvando-a em uma variável.

```Arduino

String minhaString = "Olá, mundo!";
```

Agora, para encontrar o comprimento dessa string, podemos usar o método `length()`, que é nativo do Arduino. Ele retornará o número de caracteres da string, incluindo espaços.

```Arduino
int length = minhaString.length();
Serial.println(length);
```

Isso imprimirá "12" no monitor serial, já que a string possui 12 caracteres. Podemos usar esse método em qualquer string que criarmos em nosso código.

## Mergulho profundo
Agora, vamos entender como o método `length()` funciona por trás das cenas. A verdade é que, quando estamos trabalhando com strings em Arduino, estamos trabalhando com matriz de caracteres (`char[]`). O método `length()` simplesmente contador o número de caracteres armazenados na matriz. Isso significa que ele é sensível a espaços vazios e caracteres especiais.

Outro aspecto importante é que o tamanho da string é limitado pela memória disponível no Arduino. Se tentarmos criar uma string muito grande, podemos acabar com um erro de esgotamento de memória.

## Veja também
- [Documentação oficial do Arduino sobre string](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [Tutorial de programação em Arduino (em português)](https://notafiscal.net.br/blog/tutorial-de-programacao-para-arduino-em-portugues/)
- [Fórum de discussão para programadores em Arduino (em português)](http://www.arduino.com.br/)

O conhecimento sobre como encontrar o comprimento de uma string em Arduino é essencial para qualquer programador. Com ele, você poderá criar projetos mais complexos e manipular dados de forma mais eficiente. Continue explorando o mundo da programação em Arduino e veja o que mais pode ser feito com essa habilidade!