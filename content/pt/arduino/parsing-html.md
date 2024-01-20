---
title:                "Analisando HTML"
html_title:           "Arduino: Analisando HTML"
simple_title:         "Analisando HTML"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/parsing-html.md"
---

{{< edit_this_page >}}

## O que é & Por quê?

Analisar HTML implica em desmontar uma página da web, identificando corretamente seus tags e o conteúdo de cada tag. Programadores fazem isso para extrair informações úteis, automatizar tarefas da web e integrar aplicações.

## Como fazer:

Para extrair informações de uma página da web usando Arduino, utilizaremos a biblioteca Ethernet e a biblioteca arduino-html-parser. Aqui está um exemplo básico:

```Arduino
#include <Ethernet.h>
#include <HTMLParser.h>

EthernetClient client;
HTMLParser html_parser((char *)EthernetClient::available, (char *)EthernetClient::read);

void setup()
{
  html_parser.begin();
  // conectar e realizar uma requisição GET
}

void loop()
{
  if (client.available()) {
    char c = client.read();
    html_parser.parse(c);
  }
}
```

Na saída, você verá os tags HTML corretamente identificados.

## Mergulho profundo

Analisar HTML tem uma história interessante. Durante os primeiros dias da web, as páginas eram estáticas e a análise de HTML era um pouco inútil. No entanto, com a chegada das páginas dinâmicas, a análise do HTML tornou-se uma importante técnica de extração de dados.

Existem várias alternativas para a biblioteca arduino-html-parser, tais como HtmlStreamParser e HtmlParserGeneratorSlave. Embora sejam bastante eficazes, essas bibliotecas não são tão otimizadas quanto a biblioteca arduino-html-parser.

A implementação da análise HTML no Arduino é bastante simples. Ela envolve a leitura do HTML via Ethernet, usando uma variável de tipo 'char' para armazenar cada caracter individual e, em seguida, a biblioteca de análise HTML identifica corretamente os tags e o conteúdo.

## Veja também

Para conhecer mais sobre o assunto, visite os links abaixo:

- [Documentação Arduino](https://www.arduino.cc/reference/pt/)
- [Biblioteca Ethernet Arduino](https://www.arduino.cc/en/Reference/Ethernet)