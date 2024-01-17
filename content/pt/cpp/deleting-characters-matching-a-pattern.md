---
title:                "Excluindo caracteres que correspondem a um padrão"
html_title:           "C++: Excluindo caracteres que correspondem a um padrão"
simple_title:         "Excluindo caracteres que correspondem a um padrão"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## O que & Por que?

A exclusão de caracteres que correspondem a um determinado padrão é o processo de remover caracteres específicos dentro de uma string ou sequência de texto de acordo com um padrão predefinido. Isso é útil para manipular e limpar dados em programas de processamento de texto, como corrigir erros de digitação ou remover caracteres indesejados. Os programadores realizam essa tarefa para criar strings mais limpas e padronizadas que possam ser processadas por seus programas de forma mais eficiente.

## Como fazer:

Para excluir caracteres que correspondam a um determinado padrão em C++, você pode usar a biblioteca "regex" para trabalhar com expressões regulares. Primeiro, importe a biblioteca no início do seu código. Em seguida, crie um objeto de expressão regular usando o padrão desejado e passe-o como argumento para a função "regex_replace". Por exemplo, para excluir todos os números de uma string, o código seria semelhante ao seguinte:

```C++
#include <regex>
using namespace std;

string original_str = "123abc456def789";
regex pattern("\\d+");
string cleaned_str = regex_replace(original_str, pattern, "");
cout << cleaned_str << endl;
// Output: abcdef
```

## Mergulho Profundo:

A exclusão de caracteres que correspondam a um padrão foi uma das principais funcionalidades introduzidas na biblioteca <regex> do C++11. Anteriormente, os programadores precisavam usar bibliotecas de terceiros ou implementar suas próprias soluções para trabalhar com expressões regulares em suas aplicações.

Além disso, existem outras formas de lidar com a exclusão de caracteres que correspondam a um padrão em C++. Por exemplo, pode-se usar a biblioteca "string" e seus métodos para percorrer a string e remover manualmente os caracteres desejados. Entretanto, a utilização de expressões regulares é uma opção mais avançada e poderosa, uma vez que permite maior flexibilidade e abrange uma variedade maior de padrões.

A implementação da exclusão de caracteres que correspondam a um padrão em C++ é feita através da adoção do paradigma de expressões regulares. Isso envolve a criação de objetos de expressão regular, a definição de padrões por meio de símbolos e metacaracteres, e a utilização de funções específicas da biblioteca "regex" para manipular strings.

## Veja também:

- [String and Characters - C++ Documentation](https://www.cplusplus.com/reference/string/)
- [Regular Expressions - C++ Reference](https://en.cppreference.com/w/cpp/regex)