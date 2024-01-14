---
title:    "Fish Shell: Extraindo substrings"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que

Se você está trabalhando com strings em suas programações em Fish Shell, é provável que, em algum momento, precise extrair partes específicas dessa string. O processo de extrair uma substring é conhecido como "extracting substrings" e pode ser útil em diversas situações, como manipulação de dados ou formatação de saída.

## Como fazer

Para extrair uma substring em Fish Shell, você pode utilizar o comando `string sub`. Por exemplo, se tivermos a string "lorem ipsum dolor sit amet", podemos extrair apenas "ipsum" utilizando a seguinte sintaxe:

```
string sub 'lorem ipsum dolor sit amet' 6 11
```

Onde o primeiro argumento é a string original, o segundo é o índice de início da substring e o terceiro é o índice de fim (incluindo o último caractere).

Ou seja, `string sub 'lorem ipsum dolor sit amet' 6 11` extrairá a substring "ipsum" da string original.

Além disso, você também pode utilizar esse comando em conjunto com outras ferramentas do Fish Shell, como o `echo` para imprimir a saída, ou redirecionar a saída para um arquivo. Veja um exemplo abaixo:

```
echo (string sub 'lorem ipsum dolor sit amet' 6 11) > output.txt
```

Nesse caso, a saída da string "ipsum" será redirecionada para o arquivo "output.txt".

## Aprofundando

O comando `string sub` pode ser usado de diversas maneiras, permitindo extrair não apenas uma substring específica, mas também múltiplas substrings. Além disso, também é possível utilizar expressões regulares no lugar dos índices, o que permite extrair partes da string que correspondam a um determinado padrão.

Outra funcionalidade interessante é a possibilidade de utilizar variáveis no lugar da string original, o que proporciona maior flexibilidade e dinamismo em suas programações.

Para saber mais sobre o comando `string sub` e todas suas funcionalidades, confira a documentação oficial do Fish Shell.

## Veja também

- [Documentação oficial do Fish Shell](https://fishshell.com/docs/current/cmds/document.html)
- [Tutorial sobre expressões regulares no Fish Shell](https://medium.com/@razncex/my-fish-shell-config-part-3-extracting-substrings-with-regex-groups-8edec2a01a85)
- [Exemplos de utilização do comando `string sub`](https://fishshell.com/docs/current/cmds/string-sub.html)