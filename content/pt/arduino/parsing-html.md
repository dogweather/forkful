---
title:                "Analisando html"
html_title:           "Arduino: Analisando html"
simple_title:         "Analisando html"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/parsing-html.md"
---

{{< edit_this_page >}}

## O que é e por que fazer?
Parsear HTML é basicamente o processo de analisar e extrair informações de um código HTML. Isso é muito utilizado pelos programadores para automatizar tarefas, como extrair dados de uma página da web ou criar um web scraper.

## Como fazer:
Para fazer o parseamento de HTML em um projeto Arduino, você precisará da biblioteca "Arduino HTML Parser". Ela pode ser facilmente instalada através do Gerenciador de Bibliotecas da IDE do Arduino. Depois de instalada, você pode utilizar a função "parse()" para analisar o código HTML.

```
ArduinoHTMLParser myParser; //cria uma instância do parser
myParser.parse(htmlCode); //faz o parse do código HTML fornecido
while (myParser.next()) { //loop que percorre as tags
  if (myParser.isTag() && myParser.getTag() == "p") { //verifica se a tag é <p>
    Serial.println(myParser.getValue()); //imprime o conteúdo da tag
  }
}
```

## Mergulho Profundo:
Fazer o parseamento de HTML foi um grande avanço no mundo da programação, permitindo que os programadores utilizem informações da web de maneira mais eficiente e automatizada. Existem outras formas de fazer o parseamento de HTML, como por exemplo utilizando expressões regulares, mas a biblioteca "Arduino HTML Parser" é uma opção simples e eficaz para projetos Arduino.

## Veja também:
- Documentação da biblioteca "Arduino HTML Parser": https://github.com/arduino-libraries/ArduinoHTMLParser
- Exemplos de projetos utilizando a função "parse()" da biblioteca: https://github.com/arduino-libraries/ArduinoHTMLParser/tree/master/examples