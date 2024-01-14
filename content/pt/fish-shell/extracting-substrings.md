---
title:                "Fish Shell: Extraindo subtrings"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

# Por que extrair substrings com Fish Shell?

Se você é um programador iniciante ou experiente, sempre haverá situações em que você precisará extrair uma parte de uma string maior para realizar uma tarefa específica. Felizmente, com o Fish Shell, você pode fazer isso facilmente usando a função `string sub`.

## Como fazer?

Para extrair uma substring usando o Fish Shell, é necessário seguir alguns passos simples. Primeiramente, você precisa ter a string original e também saber a posição inicial e final da substring que deseja extrair. Em seguida, pode usar a função `string sub` para realizar a extração. Veja o exemplo abaixo:

```Fish Shell
set minha_string "Olá, mundo!"
echo (string sub $minha_string 5 10)
```

Esse código irá extrair a substring "mundo" da string original e imprimirá no terminal. Note que a contagem de posição começa em 1, então a posição inicial é 5 e a posição final é 10.

O código acima irá produzir a seguinte saída:

```Shell
mundo
```

Além disso, você também pode usar a função `string length` para determinar o tamanho total da string original, facilitando a definição da posição final da substring.

## Explorando mais a fundo

A função `string sub` também possui opções adicionais para manipular o tamanho da substring extraída. Por exemplo, você pode definir um tamanho máximo para a substring, que será ignorado caso a string original seja menor. Também é possível definir um tamanho negativo, que irá contar a partir do final da string original. Para saber mais sobre essas opções e outras funções de manipulação de strings disponíveis no Fish Shell, consulte a documentação oficial.

# Veja também

- [Documentação oficial do Fish Shell para a função `string sub`](https://fishshell.com/docs/current/cmds/string_sub.html)
- [Tutorial sobre manipulação de strings com o Fish Shell](https://towardsdatascience.com/using-loops-and-string-functions-to-print-out-the-meaning-of-life-6aafeccc093d)
- [Outras funções úteis do Fish Shell para manipulação de strings](https://linuxhint.com/6-useful-fish-shell-string-operations/)