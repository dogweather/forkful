---
title:                "Encontrando o comprimento de uma string"
html_title:           "Fish Shell: Encontrando o comprimento de uma string"
simple_title:         "Encontrando o comprimento de uma string"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por que

Você provavelmente já precisou descobrir a quantidade de caracteres em uma frase ou palavra. Talvez porque você precisa garantir que seu texto se encaixe em determinado espaço, ou apenas por curiosidade. Independentemente do motivo, saber o tamanho de uma string é uma habilidade essencial na programação.

## Como fazer

Para encontrar o tamanho de uma string utilizando o Fish Shell, é muito simples. Basta utilizar o comando `begin` seguido de `string length` e a string desejada, tudo dentro de um ```Fish Shell code block```. Veja um exemplo abaixo:

```Fish Shell
begin
string length "Olá, mundo!"
```

Isso retornará o valor 12, indicando que a string "Olá, mundo!" possui 12 caracteres, incluindo espaços e pontuações. Você pode testar com outras strings para ver o resultado.

## Mergulho profundo

Para entender melhor como o comando `string length` funciona, é preciso entender como o Fish Shell lida com strings. No Fish, strings são tratadas como uma sequência de caracteres, onde cada caractere tem um valor numérico atribuído a ele. Esse valor determina quantos bytes o caractere ocupa na memória.

Então, quando utilizamos `string length` estamos pedindo ao Fish que conte quantos caracteres existem em uma determinada sequência e nos retorne esse valor.

## Veja também

Para mais informações sobre o Fish Shell e suas funcionalidades, confira os links abaixo:

- [Fish Shell Documentação](https://fishshell.com/docs/current/index.html)
- [Fish Shell Tutorial](https://fishshell.com/docs/current/tutorial.html)
- [Fish Shell Cheatsheet](https://gist.github.com/wolever/5596964)