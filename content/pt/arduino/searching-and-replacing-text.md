---
title:                "Arduino: Procurando e substituindo texto"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que?

Se você é um programador iniciante ou experiente, é provável que já tenha se deparado com a necessidade de fazer alterações em um grande bloco de texto. Isso pode ser uma tarefa tediosa e demorada se feito manualmente. Felizmente, existe uma maneira mais eficiente de realizar essa tarefa: a busca e substituição de texto. Neste artigo, vamos explorar como realizar essa operação em linguagem Arduino.

## Como fazer:

A busca e substituição de texto em Arduino pode ser feita usando a função `replace()` da biblioteca String. Primeiro, você precisa criar uma variável do tipo `String` para armazenar o texto original. Em seguida, chame a função `replace()` e passe como parâmetros o texto a ser substituído e o texto de substituição. Veja o exemplo abaixo:

```
Arduino String texto = "Reunindo-se com o Arduino é muito divertido!";
texto.replace("divertido", "fácil");
```
O código acima substituirá a palavra "divertido" por "fácil" na variável `texto`, resultando no seguinte output: "Reunindo-se com o Arduino é muito fácil!".

## Detalhando:

A função `replace()` pode ser usada para substituir todas as ocorrências de uma determinada palavra ou caractere em um texto. Por padrão, a substituição será feita em todas as ocorrências, mas você também pode limitar a quantidade de substituições passando um terceiro parâmetro opcional. Além disso, a função é case-sensitive, ou seja, ela diferencia maiúsculas e minúsculas.

## Veja também:

- [Documentação oficial da função `replace()`](https://www.arduino.cc/reference/en/language/functions/string/functions/replace/)
- [Tutorial sobre como usar a função `replace()`](https://randomnerdtutorials.com/arduino-replace-string-function/)
- [Exemplo prático de uso da função `replace()`](https://www.instructables.com/id/Arduino-Text-Replace/)