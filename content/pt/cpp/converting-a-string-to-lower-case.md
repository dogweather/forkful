---
title:    "C++: Convertendo uma string para minúsculas"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por que

Converter uma string para letras minúsculas é uma tarefa essencial em muitos projetos de programação. Isso nos permite padronizar e comparar strings de maneira mais fácil e eficiente.

## Como fazer

Abaixo estão algumas maneiras de converter uma string para letras minúsculas usando a linguagem de programação C++.

### Usando a função `transform` do STL

```C++
// Cabeçalhos necessários
#include <iostream>
#include <string>
#include <algorithm>

using namespace std;

int main() {
    // String de exemplo
    string palavra = "Exemplo";

    // Usando a função transform para converter a string para letras minúsculas
    transform(palavra.begin(), palavra.end(), palavra.begin(), ::tolower);

    // Imprimindo a string modificada
    cout << palavra;

    return 0;
}

// Saída: exemplo
```

### Usando a função `tolower` do header `cctype`

```C++
// Cabeçalhos necessários
#include <iostream>
#include <string>
#include <cctype>

using namespace std;

int main() {
    // String de exemplo
    string palavra = "Exemplo";

    // Convertendo cada caractere da string para letras minúsculas
    for (char& c : palavra) {
        c = tolower(c);
    }

    // Imprimindo a string modificada
    cout << palavra;

    return 0;
}

// Saída: exemplo
```

## Mergulho profundo

Existem algumas coisas importantes a serem consideradas ao converter uma string para letras minúsculas. Primeiro, é preciso ter cuidado ao escolher a função de conversão para garantir que ela funcione corretamente com a codificação de caracteres do seu projeto. Além disso, em alguns casos, espaços e pontuação podem não ser convertidos corretamente para suas formas minúsculas, então é importante verificar e tratar esses casos manualmente, se necessário.

## Veja também

- [Referência da função `transform` do STL em cppreference.com](https://en.cppreference.com/w/cpp/algorithm/transform)
- [Referência da função `tolower` do header `cctype` em cppreference.com](https://en.cppreference.com/w/cpp/string/byte/tolower)