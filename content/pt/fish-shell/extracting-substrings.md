---
title:    "Fish Shell: Extraindo subcadeias"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Por que utilizar a extração de substrings em Fish Shell?

A extração de substring, ou seja, a obtenção de uma parte específica de uma string, pode ser útil em diversas situações na programação com Fish Shell. Por exemplo, se você precisa manipular dados em um formato específico ou deseja remover determinados caracteres de uma string, a extração de substrings pode ser uma ótima solução.

## Como fazer a extração de substrings em Fish Shell

Para realizar essa tarefa em Fish Shell, você pode utilizar o comando `string sub`, seguido do índice do primeiro caractere e do último caractere que deseja extrair. Por exemplo, se você tem uma string chamada `frase` e deseja extrair apenas a primeira palavra, o código ficaria assim:

```
frase="Olá, tudo bem?"

string sub $frase 1 3
```

O comando acima irá extrair os caracteres da posição 1 até a posição 3, ou seja, `Olá`. Vale ressaltar que o primeiro caractere de uma string tem índice 1, e não 0.

Você também pode utilizar o comando `string match` para extrair substrings que correspondam a um padrão específico. Por exemplo, se você tem uma string que contém um endereço de e-mail e deseja obter apenas o domínio, você pode usar o seguinte código:

```
email="usuario@dominio.com"

string match --regex '\@(.+)$' $email
```

O resultado será `dominio.com`, que é a parte da string que corresponde ao padrão `\@(.+)$`, que significa "o caractere '@' seguido de um ou mais caracteres até o final da string".

## Aprofundando na extração de substrings em Fish Shell

Para quem deseja se aprofundar mais neste assunto, é possível utilizar wildcards e expressões regulares com o comando `string match` para extrair substrings ainda mais específicas. Além disso, o Fish Shell também possui outras ferramentas para manipulação de strings, como o comando `string join` e a função `string split`.

A extração de substrings também pode ser útil no processo de substituição de caracteres em uma string, por exemplo, utilizando o comando `string replace`.

## Veja também

- [Documentação oficial do Fish Shell sobre strings](https://fishshell.com/docs/current/cmds/string.html)
- [Exemplos práticos de extração de substrings em Fish Shell](https://fishshell.com/docs/current/tutorial.html#tut_special_variables)