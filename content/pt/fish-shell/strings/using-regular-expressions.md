---
title:                "Usando expressões regulares"
aliases: - /pt/fish-shell/using-regular-expressions.md
date:                  2024-02-03T19:16:48.777950-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usando expressões regulares"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Que?

Expressões regulares (regex) no Fish Shell permitem buscar, combinar e manipular strings com base em padrões específicos. Programadores utilizam regex para tarefas como validação de entrada, análise e processamento de texto, pois oferece uma maneira compacta e poderosa de especificar padrões de texto complexos.

## Como fazer:

Embora o próprio Fish Shell não tenha um comando interno para regex, ele usa efetivamente comandos externos como `grep`, `sed` e `awk` que suportam regex, permitindo incorporar operações de regex nos seus scripts.

### Combinação de Padrões Básicos com `grep`
Buscar por linhas em um arquivo que combinem com um padrão:

```fish
grep '^[0-9]+' myfile.txt
```

Este comando encontra linhas que começam com um ou mais dígitos em `myfile.txt`.

### Extraindo & Substituindo com `sed`
Extrair números de telefone de um arquivo:

```fish
sed -n '/\([0-9]\{3\}\)-\([0-9]\{3\}\)-\([0-9]\{4\}\)/p' contacts.txt
```

Substituir todas as ocorrências de "foo" por "bar" em `data.txt`:

```fish
sed 's/foo/bar/g' data.txt
```

### Usando `string` para Regex Básico
O comando `string` do Fish Shell suporta operações simples de regex como combinar e substituir:

Combinar um padrão em uma string:

```fish
echo "fish 3.1.2" | string match -r '3\.[0-9]+\.[0-9]+'
```
Saída:
```
3.1.2
```

Substituir dígitos após 'fish' por 'X.X.X':

```fish
echo "Welcome to fish 3.1.2" | string replace -ra '([fish]+\s)[0-9\.]+' '$1X.X.X'
```
Saída:
```
Welcome to fish X.X.X
```

### Combinações Avançadas com `awk`
Imprimir a segunda coluna de dados onde a primeira coluna combina com um padrão específico:

```fish
awk '$1 ~ /^a[0-9]+$/ {print $2}' datafile
```

Este comando procura por linhas em `datafile` onde a primeira coluna começa com um "a" seguido de um ou mais dígitos e imprime a segunda coluna.

Integrando esses comandos externos, programadores do Fish Shell podem aproveitar todo o poder das expressões regulares para tarefas complexas de manipulação de texto, aprimorando as capacidades nativas do shell.
