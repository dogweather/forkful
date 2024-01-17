---
title:                "Lendo argumentos da linha de comando."
html_title:           "Arduino: Lendo argumentos da linha de comando."
simple_title:         "Lendo argumentos da linha de comando."
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Comandos de Linha: O que são e para que servem?

Se você já ouviu falar sobre programação, provavelmente também já deve ter ouvido falar em "comandos de linha" ou "argumentos de linha de comando". Mas o que eles são exatamente e por que os programadores se preocupam com eles? 

## O que & Porquê?

Em termos simples, um comando de linha é uma entrada de texto inserida pelo usuário na linha de comando do Arduino, que pode ser utilizada para modificar o comportamento do programa em execução. Os programadores usam comandos de linha para tornar seus programas mais flexíveis e personalizáveis, já que é possível alterar sua execução sem ter que recompilar o código.

## Como fazer:

Para ler os comandos de linha em um programa Arduino, é necessário seguir alguns passos simples:
```
ArduinoSerialParser serialParser; //cria um objeto serialParser
void setup() {
  Serial.begin(9600); //inicia a comunicação serial
}
void loop() {
  if (Serial.available()) { //verifica se há comandos disponíveis
    //lê o comando e o armazena em uma variável
    String command = serialParser.readString(Serial);
    //faz algo com o comando lido
    Serial.println(command);
  }
}
```

## Aprofundando:

Os comandos de linha não são uma invenção recente, e são utilizados há décadas em diversas linguagens de programação diferentes. Algumas linguagens oferecem suporte nativo para a leitura de comandos de linha, enquanto outras precisam de bibliotecas externas, como é o caso do Arduino.

Além disso, existem outras formas de modificar o comportamento de um programa sem precisar alterar o código, como por exemplo, a utilização de variáveis de ambiente ou a passagem de parâmetros via linha de comando. No entanto, os comandos de linha são uma das formas mais simples e diretas de realizar essa tarefa.

## Veja também:

Para saber mais sobre comandos de linha e como utilizá-los em seus programas Arduino, recomendamos as seguintes fontes:

- [Tutorial oficial do Arduino sobre comunicação serial](https://www.arduino.cc/en/Tutorial/ReadASCIIString)
- [Uso de comandos de linha em programação C](https://www.gnu.org/software/make/manual/html_node/Command-Line.html)
- [Biblioteca SerialCommand para leitura de comandos de linha](https://github.com/kroimon/Arduino-SerialCommand)