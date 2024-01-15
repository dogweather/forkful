---
title:                "Utilizando expressões regulares"
html_title:           "C++: Utilizando expressões regulares"
simple_title:         "Utilizando expressões regulares"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por que utilizar expressões regulares?

As expressões regulares são uma ferramenta poderosa para manipulação de texto em programas C++. Com elas, é possível realizar uma série de operações, desde validação de strings até a manipulação de grandes quantidades de dados.

## Como usar expressões regulares em C++

Para utilizar expressões regulares em C++, é necessário incluir a biblioteca "regex". Em seguida, é preciso definir um objeto do tipo "regex" e atribuir a ele a expressão regular desejada. Por exemplo:

```C++
#include <regex>
#include <iostream>
using namespace std;

int main()
{
    // Definindo a expressão regular
    regex re("([a-z]+)@(\\w+\\.\\w+)(\\.\\w+)?");

    // Criando uma string para testar a expressão regular
    string email = "exemplo@dominio.com";

    // Realizando a pesquisa na string de acordo com a expressão regular
    smatch matches;
    regex_search(email, matches, re);

    // Imprimindo o resultado da busca
    cout << matches[1].str() << "@" << matches[2].str() << matches[3].str();

    return 0;
}
```

### Saída:

```
exemplo@dominio.com
```

## Mergulhando mais fundo

Expressões regulares permitem o uso de metacaracteres para facilitar a definição de padrões de busca. Alguns exemplos são:

- `.`: qualquer caractere
- `*`: zero ou mais ocorrências do caractere anterior
- `+`: uma ou mais ocorrências do caractere anterior
- `?`: zero ou uma ocorrência do caractere anterior
- `[ ]`: conjunto de caracteres permitidos
- `[^]`: conjunto de caracteres proibidos
- `^`: início da string
- `$`: fim da string

Além disso, é possível utilizar quantificadores para definir o número de ocorrências desejado, como `*` (zero ou mais), `+` (uma ou mais), `?` (zero ou uma), `{n}` (exatamente n), `{n,}` (pelo menos n) e `{n,m}` (entre n e m).

## Veja também

- [Documentação da biblioteca regex em C++](https://en.cppreference.com/w/cpp/regex)
- [Tutorial sobre expressões regulares em C++](https://www.regular-expressions.info/cpp.html)
- [Ferramenta online para testar expressões regulares](https://regex101.com/)