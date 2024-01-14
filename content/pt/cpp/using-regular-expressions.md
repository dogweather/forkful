---
title:                "C++: Usando expressões regulares"
simple_title:         "Usando expressões regulares"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por que usar expressões regulares em C++?

As expressões regulares são uma ferramenta poderosa para manipulação e busca de padrões de texto em um programa. Com seu uso, é possível economizar tempo e esforço ao escrever códigos para validação, extração e substituição de informações em uma string.

## Como usar expressões regulares em C++?

Para usar expressões regulares em C++, primeiro é necessário incluir a biblioteca "regex". Em seguida, podemos criar um objeto que irá representar a expressão regular desejada. Por exemplo, para procurar todos os números em uma string, podemos criar uma expressão regular que busque por dígitos usando o caractere especial "\d". Depois de criar o objeto, podemos utilizá-lo para procurar padrões em strings usando a função "regex_search".

```C++
#include <regex>
#include <iostream>

using namespace std;

int main() {
    // Criando a expressão regular para buscar números
    regex numeros("\\d+");

    // String para testar
    string texto = "Este é um texto com 123 números";

    // Procurando por padrões na string
    // Se encontrar, imprime "Padrão encontrado"
    if (regex_search(texto, numeros)) {
        cout << "Padrão encontrado" << endl;
    }

    return 0;
}
```

A saída do código acima será "Padrão encontrado", pois a expressão regular encontrou os números "123" na string.

## Mergulho profundo em expressões regulares

Além de buscar padrões em uma string, as expressões regulares também podem ser utilizadas para validação e substituição de informações. Com o auxílio de metacaracteres e quantificadores, é possível criar expressões regulares mais complexas que atendam a diferentes requisitos.

Outra funcionalidade interessante é utilizar grupos de captura para extrair informações específicas de uma string. Esses grupos são delimitados por parênteses na expressão regular e permitem acessar as informações encontradas em cada grupo individualmente.

É importante ter em mente também que existem diferenças nas expressões regulares suportadas por diferentes linguagens de programação, por isso é sempre bom consultar a documentação específica para a linguagem que está sendo utilizada.

## Veja também

- [Documentação oficial do C++ para expressões regulares](https://en.cppreference.com/w/cpp/regex)
- [Tutorial completo de expressões regulares em C++](https://www.regular-expressions.info/cpp.html)
- [Livro: Mastering Regular Expressions](https://www.amazon.com.br/dp/B08NTM7YCG/ref=dp-kindle-redirect?_encoding=UTF8&btkr=1)