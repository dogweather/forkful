---
title:                "Arduino: Extraindo subcadeias"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que extrair substrings no Arduino?

Muitas vezes, quando estamos trabalhando com strings no Arduino, precisamos manipulá-las de forma mais específica. É nesse momento que a extração de substrings se torna útil. Ao invés de trabalhar com a string inteira, podemos selecionar apenas um pedaço dela e utilizá-lo para realizar determinada tarefa.

## Como fazer isso no Arduino

Para extrair substrings no Arduino, podemos utilizar a função `substring()`. Ela recebe dois parâmetros: o índice inicial e o índice final desejado para a substring. Podemos também salvar a substring em uma nova variável, para facilitar seu uso posteriormente.

```
Arduino String string = "Exemplo de string";
Arduino String substring = string.substring(3, 9);

Serial.println(substring); // Saída: emplo d
```

Neste exemplo, selecionamos a partir do quarto caractere até o nono da string original e salvamos essa substring na variável `substring`.

## Mergulhando mais fundo

É importante lembrar que o índice inicial da substring é contado a partir do zero. Além disso, podemos usar um único parâmetro para extrair uma substring a partir de um índice até o final da string, como mostrado no exemplo a seguir:

```
Arduino String string = "Outro exemplo de string";
Arduino String substring = string.substring(6);

Serial.println(substring); // Saída: exemplo de string
```

Também é possível utilizar valores negativos para os parâmetros, como `substring(-3, -1)` para extrair os três últimos caracteres da string original.

## Veja também

- Documentação oficial do Arduino para a função substring: https://www.arduino.cc/reference/pt/language/variables/data-types/string/functions/substring/
- Tutorial sobre manipulação de strings no Arduino: https://www.filipeflop.com/blog/manipulando-strings-no-arduino/