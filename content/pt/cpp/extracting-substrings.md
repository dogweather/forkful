---
title:                "Extraindo subcadeias"
html_title:           "C++: Extraindo subcadeias"
simple_title:         "Extraindo subcadeias"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

## O que & Por que?

Extrair substrings é um processo comum na programação que envolve a divisão de uma string em partes menores. Isso pode ser feito com o objetivo de manipular as informações de forma mais eficiente ou para obter apenas uma parte específica da string. Programadores costumam usar esse recurso para tarefas como validação de dados, formatação de texto ou comparação de strings.

## Como fazer:

Para extrair substrings em C++, você pode usar a função `substr()` da biblioteca padrão `string`. Veja um exemplo de código:

```
#include <iostream>
#include <string>

using namespace std;

int main() {
    string nome = "Alice";
    string sobrenome = nome.substr(1, 3);
    cout << sobrenome << endl;
    
    return 0;
}
```

**Saída:**
```
lic
```

Neste exemplo, definimos uma string `nome` com o valor "Alice" e usamos a função `substr()` para extrair a segunda e terceira letra da palavra. O primeiro parâmetro informa a posição inicial e o segundo parâmetro é a quantidade de caracteres a serem extraídos.

## Deep Dive:

A função `substr()` foi introduzida na versão C++98 e está disponível na biblioteca `string`, que foi criada a partir da versão C++11. Alguns programadores preferem usar a função `substr()` da biblioteca `cstring`, que é mais antiga e foi herdada do C. Ela funciona da mesma forma, mas requer que o programador gerencie manualmente a posição do ponteiro que aponta para a string.

Existem também outras formas de extrair substrings em C++, como usar laços para percorrer a string e separar os caracteres desejados. No entanto, a função `substr()` geralmente é mais eficiente e recomendada para esse propósito.

## Veja também:

- [Documentação oficial da função `substr()` do C++](https://en.cppreference.com/w/cpp/string/basic_string/substr)
- [Tutorial sobre strings em C++](https://www.learncpp.com/cpp-tutorial/strings/)
- [Perguntas frequentes sobre strings em C++](https://www.learncpp.com/cpp-tutorial/faq-strings/)