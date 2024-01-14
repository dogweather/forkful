---
title:    "Bash: Utilizando expressões regulares"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/bash/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por Que Usar Expressões Regulares?

Expressões regulares são uma ferramenta poderosa para lidar com textos em Bash. Elas permitem que você procure por padrões específicos em um texto, tornando o processo de manipulação e extração de informações mais eficiente. Se você trabalha com Bash e precisa lidar com textos de forma constante, aprender sobre expressões regulares pode ser extremamente útil.

## Como Usar Expressões Regulares em Bash

Em Bash, as expressões regulares são escritas entre barras (/). Por exemplo, se você quiser encontrar a palavra "hello" em um texto, você pode usar a expressão regular ```/hello/```. Mas as expressões regulares vão muito além disso e podem incluir caracteres especiais, classes de caracteres e metacaracteres.

Vamos ver alguns exemplos:

- Para encontrar todas as palavras que começam com a letra "h" em um texto, você pode usar a expressão regular ```/h[a-z]*/```. O caractere [] indica uma classe de caracteres, enquanto o * significa "0 ou mais ocorrências do caractere anterior". Então, essa expressão encontraria palavras como "hello", "house", "happy", etc.

- Para encontrar todas as ocorrências de números em um texto, você pode usar a expressão regular ```/[0-9]+/```. O + significa "1 ou mais ocorrências do caractere anterior", então essa expressão encontraria números como 123, 456, 789, etc.

- Você também pode usar o metacaractere "." que representa qualquer caractere. Por exemplo, a expressão regular ```/h.ll./``` encontraria palavras como "hello", "hills", "hells", etc.

Para testar as expressões regulares, você pode usar um editor de texto com suporte a expressões regulares, como o Visual Studio Code ou o Sublime Text. Ou, se você preferir, pode testá-las diretamente no seu terminal de Bash com o comando ```grep```. Por exemplo, para encontrar todas as linhas de um arquivo que contenham a palavra "hello", você pode usar o comando ```grep hello arquivo.txt```.

## Mergulhando Mais Fundo em Expressões Regulares

Como mencionado anteriormente, existem vários caracteres especiais e classes de caracteres que podem ser usados em expressões regulares. Alguns exemplos incluem:

- "\d" representa qualquer dígito, equivalente a [0-9].
- "\w" representa qualquer caractere alfanumérico, equivalente a [a-zA-Z0-9].
- "\s" representa um espaço em branco, incluindo espaços, tabs e quebras de linha.

Além disso, você pode usar os operadores lógicos "|" (OR) e "?" (opcional) em suas expressões regulares.

Para se aprofundar ainda mais, você pode consultar a documentação oficial do Bash ou ler artigos e tutoriais disponíveis na web. Praticar também é essencial para se familiarizar com as expressões regulares e aproveitar ao máximo seu uso em Bash.

## Veja também

- [Documentação do Bash sobre Expressões Regulares](https://www.gnu.org/software/bash/manual/html_node/Pattern-Matching.html)
- [Artigo do Linuxize sobre Expressões Regulares em Bash](https://linuxize.com/post/how-to-use-regular-expressions-regex-in-bash/)
- [Site Regex101 para testar e experimentar com expressões regulares](https://regex101.com/)