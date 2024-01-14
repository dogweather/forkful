---
title:                "Arduino: Extraindo subcadeias de caracteres"
simple_title:         "Extraindo subcadeias de caracteres"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que extrair substrings?

Extrair substrings pode ser útil ao trabalhar com textos longos em projetos de Arduino. Pode permitir que você manipule efetivamente partes específicas do texto, em vez de ter que lidar com toda a string de uma só vez.

## Como fazer

Para extrair uma substring em Arduino, você precisará usar a função ```substring()```. Esta função aceita dois argumentos: o índice inicial e o tamanho da substring desejada. Por exemplo, para extrair uma substring de 5 caracteres a partir do índice 2, você usaria o seguinte código:

```
String texto = "Olá mundo!";
String sub_texto = texto.substring(2,5);
Serial.println(sub_texto);
```

A saída deste código seria "á mu", já que a substring começa no segundo caractere (índice 2) e tem um tamanho de 5 caracteres.

## Mergulho profundo

Se você quiser extrair uma substring com base em um padrão específico, em vez de índices numéricos, pode utilizar a função ```indexOf()``` para encontrar a posição do padrão e, em seguida, utilizar isso como o índice inicial para a função ```substring()```. Por exemplo:

```
String texto = "Meu nome é João.";
int inicio = texto.indexOf("nome");
String nome = texto.substring(inicio, inicio + 4);
Serial.println(nome);
```

A saída deste código seria "nome", pois é a substring contendo o padrão "nome" na variável "texto".

## Veja também

- [Documentação da função substring() do Arduino](https://www.arduino.cc/reference/pt/language/variables/data-types/string/functions/substring/)
- [Vídeo explicando extração de substrings no Arduino](https://www.youtube.com/watch?v=irPvR2-yBCA)
- [Exemplos de código para extrair substrings em projetos de Arduino](https://www.tinkercad.com/search?q=arduino%20substring&type=circuits&sort=trend)