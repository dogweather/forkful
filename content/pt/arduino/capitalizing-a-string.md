---
title:                "Arduino: Capitalizando uma string"
simple_title:         "Capitalizando uma string"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por que capitalizar uma string?

Quando estamos trabalhando com strings em Arduino, muitas vezes precisamos manipular esses dados para que eles estejam no formato correto, como por exemplo, todas as primeiras letras em maiúsculo. Nesses casos, é importante saber como capitalizar uma string para que possamos apresentar os dados de forma eficiente.

## Como fazer?

Para capitalizar uma string em Arduino, podemos usar a função `toUpperCase()` da biblioteca `String` do Arduino. Veja abaixo um exemplo de código:

```Arduino
String texto = "capitalizar essa string";
texto.toUpperCase();
Serial.println(texto);
```

Neste exemplo, a função `toUpperCase()` irá transformar todas as primeiras letras de cada palavra em maiúsculo, resultando em "Capitalizar Essa String" ao imprimir na porta serial.

É importante ressaltar que a função `toUpperCase()` só irá alterar as letras que estão em minúsculo, mantendo as letras maiúsculas como estão. Além disso, essa função também pode ser usada com variáveis do tipo `char`.

## Mais informações sobre capitalização de strings

Uma maneira de capitalizar uma string é utilizando loops e condições para percorrer a string e alterar as letras desejadas. Porém, ao utilizar a função `toUpperCase()` do Arduino, essa tarefa se torna mais simples e eficiente.

Além disso, é importante ressaltar que a função `toUpperCase()` pode ser utilizada em conjunto com outras funções de manipulação de strings, como `substring()` e `replace()`.

## Veja também

- Documentação oficial do Arduino sobre a função `toUpperCase()`: https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/touppercase/
- Tutoriais sobre strings em Arduino: https://www.embarcados.com.br/strings-em-arduino/
- Vídeo tutorial explicando como capitalizar strings em Arduino: https://www.youtube.com/watch?v=gjQyZVAerEs