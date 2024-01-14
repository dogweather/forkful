---
title:                "Bash: Usando expressões regulares"
simple_title:         "Usando expressões regulares"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por que usar Expressões Regulares em Programação Bash?

Expressões regulares, também conhecidas como regex, são uma poderosa ferramenta para manipular texto em programação Bash. Com elas, é possível buscar por padrões específicos em um conjunto de caracteres, substituir partes de um texto e validar entradas do usuário. Isso torna as expressões regulares uma ferramenta essencial para aqueles que trabalham com processamento de texto em seus scripts Bash.

## Como usar Expressões Regulares em Bash

Para utilizar expressões regulares em Bash, é necessário utilizar o comando `grep`. Este comando é responsável por buscar por padrões em um texto e imprimir as linhas que os correspondem. Vamos ver um exemplo de como utilizá-lo:

```Bash
texto="Este texto contém palavras aleatórias como bananas, maçãs e laranjas"
echo "$texto" | grep "maçãs"
```

Nesse exemplo, a variável `texto` contém uma string que possui as palavras "bananas", "maçãs" e "laranjas". Ao utilizarmos o comando `echo` e o pipe `|` para enviar esse texto para o comando `grep`, especificamos que queremos buscar pelo padrão "maçãs". O output desse comando é a linha que contém essa palavra, ou seja, "Este texto contém palavras aleatórias como bananas, **maçãs** e laranjas".

Podemos utilizar expressões regulares mais complexas para buscar por padrões mais específicos. Por exemplo, se quisermos buscar por palavras que comecem com a letra "b", podemos utilizar o seguinte comando:

```Bash
echo "$texto" | grep "\bb"
```

Aqui, o metacaractere `\b` indica que queremos buscar palavras que comecem com a letra "b". O resultado desse comando seria a linha "texto contém palavras aleatórias como **bananas**, maçãs e laranjas".

Além do comando `grep`, também é possível utilizar expressões regulares com o comando `sed`, que é responsável por substituir partes de um texto. Combinar esses comandos com expressões regulares pode facilitar muito a manipulação de textos em seus scripts Bash.

## Mergulhando Fundo nas Expressões Regulares

As expressões regulares em Bash seguem a mesma lógica utilizada em outras linguagens de programação e sistemas operacionais. Elas incluem metacaracteres para representar diferentes tipos de caracteres e quantificadores para especificar a quantidade de ocorrências de um padrão.

Por exemplo, se quisermos buscar por um número de telefone no formato XXXX-XXXX, podemos utilizar o seguinte padrão: `\d{4}-\d{4}`. Nesse caso, o metacaractere `\d` representa qualquer dígito de 0 a 9, e o quantificador `{4}` especifica que queremos exatamente 4 dígitos.

Existem muitos metacaracteres e quantificadores diferentes que podem ser utilizados em expressões regulares. É importante estudar e praticar para dominar o uso dessas ferramentas e ser capaz de resolver problemas complexos em seus scripts Bash.

## Veja Também

- [Manual do Bash sobre Expressões Regulares](https://www.gnu.org/software/sed/manual/html_node/Regular-Expressions.html)
- [Tutorial de Expressões Regulares no Bash](https://linuxize.com/post/regular-expressions-in-bash/)
- [Exemplos de uso de Expressões Regulares em Bash](https://www.thegeekstuff.com/2009/10/unix-linux-sed-tutorial-how-to-execute-multiple-sed-commands/)