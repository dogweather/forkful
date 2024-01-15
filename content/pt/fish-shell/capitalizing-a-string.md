---
title:                "Capitalizando uma string"
html_title:           "Fish Shell: Capitalizando uma string"
simple_title:         "Capitalizando uma string"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por que

Você já se deparou com uma situação em que precisava capitalizar uma string em seu código? Talvez você esteja escrevendo uma aplicação em que a precisão é importante ou talvez você queira apenas obter uma aparência esteticamente agradável. Seja qual for o seu motivo, o Fish Shell possui uma função conveniente para capitalizar strings de forma fácil e eficiente.

## Como fazer

Para capitalizar uma string usando o Fish Shell, basta seguir os passos abaixo:

1. Abra o terminal e inicie o Fish Shell digitando `fish` no prompt.
2. Declare a string que deseja capitalizar em uma variável, por exemplo: `set my_string "exemplo"`
3. Use o comando `string capitalize` seguido pelo nome da variável, dentro de colchetes, para capitalizar sua string, por exemplo: `string capitalize $my_string`
4. Observe a saída do terminal que agora mostra o valor da variável com a primeira letra maiúscula: `Exemplo`

```
Fish Shell 3.2.2
Confira em breve mais novidades!
Type 'help' para mais informações
∫ set my_string "exemplo"
∫ string capitalize $my_string
∫ echo $my_string
Exemplo
```

## Nível avançado

Caso você queira ir um pouco além com a capitalização de strings, o Fish Shell possui outras duas funções interessantes: `string capitalize-rest` e `string capitalize-rest-words`.

A função `string capitalize-rest` irá capitalizar todas as letras após a primeira em cada palavra da sua string, enquanto a função `string capitalize-rest-words` irá capitalizar apenas as primeiras letras de cada palavra. Veja os exemplos abaixo:

```
∫ set my_string "exemplo de string"
∫ string capitalize-rest $my_string
Exemplo De String
∫ string capitalize-rest-words $my_string
Exemplo De String
```

## Veja também

- [Documentação oficial do Fish Shell](https://fishshell.com/docs/current/index.html)
- [Repositório oficial do Fish Shell no GitHub](https://github.com/fish-shell/fish-shell)
- [Tutorial: Como usar o Fish Shell](https://www.hostinger.com.br/tutoriais/fish-shell/)