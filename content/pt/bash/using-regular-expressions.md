---
title:                "Bash: Utilizando expressões regulares"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por que usar expressões regulares?

Expressões regulares são uma forma eficiente e poderosa de fazer correspondências de padrões em textos. Elas são amplamente utilizadas em programação e podem ajudar a simplificar tarefas que envolvem processamento de texto. Além disso, são uma habilidade útil para quem trabalha com dados e precisa extrair informações específicas de grandes quantidades de informações.

## Como usar expressões regulares em Bash

Para usar expressões regulares no Bash, é necessário usar o comando `grep`. Ele é responsável por buscar padrões em textos e retornar as linhas que correspondem a esses padrões. 

Por exemplo, se quisermos encontrar todas as palavras que terminam com "ção" em um arquivo de texto, podemos usar o seguinte comando:

```bash 
grep -E '\w+ção\b' texto.txt
``` 

O `-E` indica que estamos usando expressões regulares estendidas e o `\b` representa um limite de palavra. O resultado será uma lista das palavras encontradas no arquivo que terminam com "ção".

## Aprofundando-se em expressões regulares

Em Bash, é possível usar algumas opções para especificar o tipo de busca que queremos fazer com expressões regulares. Por exemplo, o `grep` possui opções como `-i` para ignorar maiúsculas e minúsculas, `-v` para encontrar linhas que não correspondem ao padrão e `-n` para mostrar o número da linha em que o padrão foi encontrado.

Além disso, é possível combinar expressões regulares com outros comandos no Bash, como usar pipes (`|`) com o `sort` para classificar os resultados encontrados pelo `grep`.

Para aprender mais sobre expressões regulares em Bash, recomenda-se a leitura da documentação oficial do `grep` e a prática de diferentes exemplos.

## Veja também

- Documentação oficial do `grep` (https://www.gnu.org/software/grep/manual/grep.html)
- Tutorial de expressões regulares em Bash (https://www.digitalocean.com/community/tutorials/using-grep-regularexpressions-to-search-for-text-patterns-in-linux)
- Cheat sheet de expressões regulares (https://www.rexegg.com/regex-quickstart.html)