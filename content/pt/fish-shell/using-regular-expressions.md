---
title:                "Fish Shell: Utilizando expressões regulares"
simple_title:         "Utilizando expressões regulares"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Porque
Se você está procurando uma maneira eficiente e poderosa de manipular e filtrar dados em seu terminal, o uso de expressões regulares é a solução ideal. Com o Fish Shell, você pode facilmente integrar expressões regulares em seu fluxo de trabalho para automatizar tarefas e tornar o seu trabalho mais eficiente.

## Como Fazer
Para começar a usar expressões regulares no Fish Shell, tudo o que você precisa fazer é usar o comando `grep`. Este comando permite que você procure por padrões em arquivos ou saídas de comando. Você também pode usar o `sed` para substituir ou manipular dados com base em seus padrões.

Veja um exemplo simples de como encontrar todas as linhas que contêm a palavra "hello" em um arquivo de texto:

```Fish Shell
grep hello arquivo.txt
```

Isso irá imprimir todas as linhas que contêm a palavra "hello" no arquivo "arquivo.txt". Agora, vamos supor que queremos substituir todas as ocorrências de "hello" por "olá". Podemos usar o `sed` da seguinte maneira:

```Fish Shell
sed -i 's/hello/olá/g' arquivo.txt
```

O `sed -i` irá substituir o texto diretamente no arquivo e o `s/padrão/substituição/g` indica que queremos substituir todas as ocorrências do padrão pelo texto de substituição.

## Mergulho Profundo
Além dos comandos `grep` e `sed`, o Fish Shell também suporta outros utilitários úteis para trabalhar com expressões regulares, incluindo `awk` e `perl`. Além disso, você também pode usar meta-caracteres e classes para tornar suas expressões regulares ainda mais poderosas.

Por exemplo, o caractere `.` pode ser usado para representar qualquer caractere, e `[]` pode ser usado para combinar qualquer caractere dentro dos colchetes. Você também pode usar `|` para delimitar opções dentro de uma expressão regular.

## Veja Também
- [Documentação do Fish Shell](https://fishshell.com/docs/current/)
- [Guia Interativo do Fish Shell](https://fishshell.com/docs/current/tutorial.html)
- [Introdução ao Expressões Regulares](https://fishshell.com/docs/current/tutorial.html#other-topics)