---
title:    "Fish Shell: Capitalizando uma string"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Por que capitalizar uma string em Fish Shell

Quando se trabalha com dados em qualquer linguagem de programação, muitas vezes é necessário modificar o formato de uma string. Isso pode incluir deixar todas as letras maiúsculas ou minúsculas ou, em alguns casos, capitalizar apenas a primeira letra. Neste post, vamos falar especificamente sobre como capitalizar uma string em Fish Shell e como isso pode ser útil em seus projetos de programação.

## Como fazer

Para capitalizar uma string em Fish Shell, podemos usar o comando `string capitalize` seguido pelo texto que desejamos capitalizar. Por exemplo, se quisermos capitalizar a string "exemplo", o código ficaria assim:

```
Fish Shell:

string capitalize "exemplo"

Saída:
Exemplo
```

Também é possível capitalizar apenas a primeira letra de cada palavra em uma string. Para isso, usamos o comando `string capitalize -a`. Veja o exemplo abaixo:

```
Fish Shell:

string capitalize -a "exemplo de texto"

Saída:
Exemplo De Texto
```

Caso queira fazer o contrário e deixar todas as letras em minúsculo, podemos usar o comando `string tolower`. Veja o exemplo:

```
Fish Shell:

string tolower "TEXTO EM CAIXA ALTA"

Saída:
texto em caixa alta
```

Por fim, é importante lembrar que esses comandos também podem ser usados em variáveis, possibilitando a capitalização de dados dinâmicos em um script.

## Aprofundando-se

Para entender melhor como a capitalização de strings funciona em Fish Shell, é importante entender que esse comando funciona usando as configuracoes de linguagem definidas no sistema operacional. Para alterar essas configuracoes, podemos acessar o arquivo `locale.conf`.

Além disso, se precisarmos capitalizar uma string com caracteres especiais, é importante ter atenção às configuracoes de codificação de caracteres, garantindo que os dados serão interpretados corretamente.

## Veja também

- [Documentação oficial do comando `string`](https://fishshell.com/docs/current/cmds/string.html)
- [Tutorial sobre o uso de variáveis em Fish Shell](https://fishshell.com/docs/current/tutorial.html#tut_variables)
- [Post sobre a manipulação de strings em Fish Shell](https://www.ostechnix.com/fish-shell-string-manipulation-tutorial/)