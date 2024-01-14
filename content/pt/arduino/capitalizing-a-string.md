---
title:    "Arduino: Capitalizando uma string"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

# Por que é importante capitalizar uma string em Arduino

Ao programar em Arduino, às vezes é necessário capitalizar uma string, ou seja, transformar todas as letras minúsculas em letras maiúsculas. Isso pode ser útil para padronizar a entrada de dados ou para criar mensagens mais legíveis para o usuário. Neste post, vamos explicar como fazer isso em poucos passos.

## Como capitalizar uma string em Arduino

Existem várias maneiras de capitalizar uma string em Arduino, mas vamos nos concentrar em uma forma simples utilizando a função `toUpperCase()`. Vamos supor que temos uma variável chamada `nome` contendo uma string em letras minúsculas e queremos transformá-la em letras maiúsculas. Podemos fazer isso da seguinte maneira:

```
Arduino nome = "exemplo";
nome.toUpperCase();

```
A saída deste código seria `EXEMPLO`, pois a função `toUpperCase()` altera a string original para que todas as letras sejam maiúsculas.

Outra opção é utilizar a biblioteca `String`, que possui uma função específica para capitalizar uma string. Veja o código abaixo:

```
Arduino nome = "exemplo";
nome = String( nome ).toUpperCase();

```
Agora, além de alterar a string original, a função `toUpperCase()` retorna uma cópia da string em letras maiúsculas.

## Aprofundando-se na capitalização de strings em Arduino

A função `toUpperCase()` é a opção mais simples para capitalizar uma string em Arduino, mas é importante entender como ela funciona por trás dos bastidores. Basicamente, essa função itera através de cada caractere da string e verifica se ele é uma letra minúscula. Se for, ele o converte para maiúsculo. Isso acontece até o final da string, e o resultado é a string original com todas as letras maiúsculas.

É importante ressaltar que essa função só funciona com caracteres no formato ASCII. Se a string contiver caracteres especiais ou acentuações, eles não serão convertidos corretamente.

## Veja também

- [Tutorial sobre String em Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [Como converter strings para inteiros em Arduino](https://www.arduino.cc/reference/en/language/functions/conversion/atoi/)
- [Tudo sobre a biblioteca String em Arduino](https://www.arduinolibraries.info/libraries/string)