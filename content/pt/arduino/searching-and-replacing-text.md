---
title:                "Buscando e substituindo texto"
html_title:           "Arduino: Buscando e substituindo texto"
simple_title:         "Buscando e substituindo texto"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que

Você já se deparou com um texto extenso e precisou alterar todas as ocorrências de uma palavra específica? Ou talvez precise passar por um código Arduino e substituir um valor por outro? A busca e substituição de texto é uma técnica comum e útil para essas situações, economizando tempo e esforço na edição manual.

## Como fazer

Para realizar a busca e substituição de texto em um código Arduino, podemos utilizar a função `replace()` da biblioteca `String`. Vamos criar um pequeno exemplo onde temos uma variável com o valor "Hello World" e precisamos substituir a palavra "World" por "Arduino".

```
Arduino
String mensagem = "Hello World"; // variável com valor inicial
mensagem.replace("World", "Arduino"); // realiza substituição
Serial.println(mensagem); // imprime o novo valor
```

A saída desse código será "Hello Arduino", mostrando que a palavra "World" foi substituída com sucesso. É importante notar que a função `replace()` modifica diretamente a variável original, portanto é importante ter atenção ao selecionar a palavra a ser substituída.

## Mais detalhes

A função `replace()` aceita três parâmetros: a subcadeia de texto a ser buscada, a subcadeia de substituição e um parâmetro booleano opcional para indicar se a busca deve ser case-sensitive (diferenciando maiúsculas de minúsculas) ou não. Além disso, também podemos utilizar o operador `+` para concatenar strings e realizar diversas substituições em um mesmo texto.

## Veja também

- [Documentação oficial da função replace()](https://www.arduino.cc/reference/pt/language/variables/data-types/string/functions/replace/)
- [Tutorial sobre String no Arduino](https://www.arduino.cc/en/Tutorial/BuiltInExamples/StringConstructors/)
- [Exemplo de uso da função replace()](https://create.arduino.cc/projecthub/AnmolSandeepNijjar/finding-and-replacing-word-with-characters-630a74?ref=user&ref_id=372482&offset=3)