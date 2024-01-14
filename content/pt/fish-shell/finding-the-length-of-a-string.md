---
title:    "Fish Shell: Encontrando o comprimento de uma string."
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Por que

Encontrar o comprimento de uma string é uma tarefa comum na programação e pode ser extremamente útil em várias situações. Através do uso da Fish Shell e algumas técnicas simples, você pode facilmente obter o comprimento de uma string em seu código.

## Como fazer

Para encontrar o comprimento de uma string, você pode usar o comando `count`. É importante lembrar que o índice de início de uma string é 1, ao invés de 0 como em outras linguagens de programação.

```Fish Shell
count "Hello World"
```

Saída: 11

Você também pode usar a função `string length` para obter o comprimento de uma string.

```Fish Shell
string length "Hello World"
```

Saída: 11

## Mergulho profundo

É importante entender que o comando `count` conta o número de caracteres em uma string, incluindo espaços em branco e pontuações. Se você quiser encontrar o número de palavras em uma string, pode usar o comando `wc` em conjunto com o filtro `words`.

```Fish Shell
echo "Este é um exemplo de string" | wc -w | awk '{print $1}'
```

Saída: 6

Além disso, se você quiser encontrar o número de linhas em uma string, pode usar o comando `wc` em conjunto com o filtro `lines`.

```Fish Shell
echo "Este é um exemplo de string" | wc -l | awk '{print $1}'
```

Saída: 1

## Veja também

- Documentação oficial da Fish Shell: https://fishshell.com/docs/current/index.html
- Tutorial do Fish Shell: https://fishshell.com/docs/current/tutorial.html
- Fórum da Fish Shell: https://github.com/fish-shell/fish-shell/issues