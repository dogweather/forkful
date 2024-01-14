---
title:                "C++: Utilizando expressões regulares"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

Por que utilizar expressões regulares em programação?

As expressões regulares são um recurso poderoso para manipular e validar texto em várias linguagens de programação. Elas permitem que os programadores realizem pesquisas e substituições complexas em cadeias de caracteres, economizando tempo e esforço. Além disso, com a ajuda das expressões regulares, é possível garantir que o texto fornecido pelo usuário esteja no formato correto antes de ser processado pelo programa.

Como utilizar expressões regulares em C++?

Para utilizar expressões regulares em C++, é necessário primeiro incluir a biblioteca <regex> no código. Em seguida, é preciso criar um objeto de expressão regular com a sintaxe ``std::regex expressao``, passando a expressão regular desejada como um parâmetro. Depois, basta utilizar a função ``std::regex_match()`` para verificar se a expressão corresponde à string fornecida. Veja um exemplo abaixo:

```
#include <regex>
#include <iostream>

using namespace std;

int main() {
    // Criando um objeto de expressão regular para verificar se uma string contém apenas números
    regex expressao("[0-9]+");

    // String de teste
    string texto = "12345";

    // Verificando se a string atende à expressão regular
    if (regex_match(texto, expressao)) {
        cout << "A string contém apenas números." << endl;
    } else {
        cout << "A string não contém apenas números." << endl;
    }

    return 0;
}
```

Neste exemplo, a expressão regular "[0-9]+" verifica se a string contém apenas números de 0 a 9. Se a expressão corresponder à string fornecida, a mensagem "A string contém apenas números" será impressa na tela.

Aprofundando-se nas expressões regulares

As expressões regulares em C++ suportam vários recursos além da simples validação de strings. Alguns desses recursos incluem:

- Caracteres curinga, como "." para qualquer caractere e "\w" para qualquer caractere alfanumérico.
- Grupos de captura, que permitem extrair partes específicas da string que corresponderam à expressão.
- Quantificadores, como "*" para zero ou mais ocorrências, "+" para uma ou mais ocorrências e "?" para uma ocorrência opcional.

Para aprender mais sobre esses recursos e como usá-los em expressões regulares em C++, confira a documentação oficial ou tutoriais online.

Veja também

- Documentação oficial sobre expressões regulares em C++: https://en.cppreference.com/w/cpp/regex
- Tutorial sobre expressões regulares em C++: https://www.tutorialspoint.com/cpp_standard_library/cpp_regex_class.htm
- Exemplos de uso de expressões regulares em C++: https://www.geeksforgeeks.org/regular-expressions-in-c-with-examples/

Esperamos que este artigo tenha sido útil para entender um pouco mais sobre o uso de expressões regulares em C++. Agora você pode explorar essa poderosa ferramenta em seus próprios projetos. Boa codificação!