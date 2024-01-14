---
title:                "Bash: Extraindo subcadeias"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que

Extração de subcadeias é uma habilidade essencial para qualquer programador de Bash. Com ela, você pode facilmente extrair trechos específicos de uma string, permitindo que você manipule e utilize dados de forma mais eficiente em seus scripts.

## Como Fazer

Para extrair uma subcadeia de uma string em Bash, você pode usar o operador de substituição de padrões `${string#substring}`. Isso irá remover a substring do início da string. Se você quiser remover a substring do final da string, você pode usar o operador `${string%substring}`.

Por exemplo, se tivermos a string `Hello World`, e quisermos extrair apenas a palavra `World`, podemos usar o seguinte código:

```Bash
string="Hello World"
substring="Hello "
echo ${string#substring}
```

Isso irá imprimir `World`, pois a substring `Hello ` será removida do início da string.

Além disso, se você quiser extrair uma subcadeia baseada em sua posição, pode usar a sintaxe `${string:position:length}`. Isso irá retornar os caracteres da string a partir da posição especificada, com o comprimento especificado. Se o comprimento não for especificado, a subcadeia irá até o final da string.

Por exemplo, se tivermos a string `Bash Programming`, e quisermos extrair apenas a palavra `Programming`, podemos usar o seguinte código:

```Bash
string="Bash Programming"
echo ${string:5}
```

Isso irá imprimir `Programming`, pois o primeiro parâmetro especifica a posição a partir da qual queremos extrair a substring.

Você também pode combinar esses métodos para extrair substrings mais complexas e realizar tarefas específicas em seus scripts.

## Mergulho Profundo

A extração de subcadeias pode ser ainda mais poderosa quando combinada com outras ferramentas do Bash, como loops e arrays. Combinando esses recursos, você pode facilmente processar grandes quantidades de dados e extrair informações relevantes diretamente de suas strings.

Além disso, existem muitas implementações diferentes de extração de substrings em Bash, permitindo que você escolha a sintaxe mais adequada para cada situação específica.

## Veja Também

- [The Bash Guide to Using Variables](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameters.html)
- [Bash String Manipulation](https://www.bashguru.com/2010/12/string-manipulation-in-bash.html)
- [AWK String Functions](https://www.tutorialspoint.com/awk/awk_string_functions.htm)