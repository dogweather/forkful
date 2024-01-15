---
title:                "Procurando e substituindo texto"
html_title:           "Bash: Procurando e substituindo texto"
simple_title:         "Procurando e substituindo texto"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que 

Você já se perguntou como seria possível substituir um termo ou parte de um texto por outro? Isso pode ser necessário para economizar tempo e tornar o processo de edição mais eficiente. E é exatamente isso que podemos fazer com Bash!

## Como fazer

Bash é uma linguagem de scripting utilizada em sistemas operacionais Linux e Unix. Para realizar uma substituição de texto, podemos utilizar o comando "sed" seguido de uma expressão regular entre aspas, indicando a substituição desejada.

```Bash
sed 's/<texto_a_ser_substituido>/<novo_texto>/g' <arquivo_origem>.txt > <arquivo_destino>.txt
```

Vamos a um exemplo prático. Digamos que queremos substituir a palavra "maçã" por "banana" em um arquivo chamado "frutas.txt". O comando ficaria da seguinte forma:

```Bash
sed 's/maçã/banana/g' frutas.txt > frutas_substituidas.txt
```

O resultado seria um novo arquivo chamado "frutas_substituidas.txt", com todas as ocorrências de "maçã" substituídas por "banana".

## Deep Dive

Além da substituição simples de palavras, é possível usar expressões regulares mais complexas com o comando "sed". Por exemplo, podemos utilizar a flag "-i" para fazer a substituição diretamente no arquivo original, sem precisar criar um novo arquivo.

Também é possível fazer a substituição em um determinado trecho de texto, especificando a linha inicial e final desejada. Isso é feito adicionando o número da linha antes do comando "s". Por exemplo, se quisermos substituir a palavra "mamão" apenas na linha 5 do arquivo, o comando seria:

```Bash
sed '5s/mamão/banana/g' frutas.txt
```

Além disso, o comando "sed" possui diversas opções que podem ser combinadas para realizar substituições mais complexas e estruturadas. Vale a pena explorar a documentação ou buscar por tutoriais para descobrir todas as possibilidades.

## Veja também

- [Documentação do comando "sed"](https://www.gnu.org/software/sed/manual/sed.html)
- [Tutorial de expressões regulares para iniciantes](https://www.digitalocean.com/community/tutorials/using-grep-regular-expressions-to-search-for-text-patterns-in-linux)
- [Comandos avançados do Bash](https://wiki.bash-hackers.org/)