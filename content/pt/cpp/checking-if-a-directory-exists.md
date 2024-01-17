---
title:                "Verificando se um diretório existe"
html_title:           "C++: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## O que & Por quê?

Verificar se um diretório existe é uma tarefa comum para programadores de C++. Isso permite que o programa execute ações específicas dependendo da existência ou não de um diretório específico. É uma maneira de garantir que o programa funcione corretamente e lide com situações inesperadas.

## Como fazer:

Para verificar se um diretório existe em C++, podemos usar a função `std::filesystem::exists()`. Esta função retorna verdadeiro se o diretório especificado existe e falso caso contrário.

```
#include <iostream>
#include <filesystem>

using namespace std;

int main() {
    string diretorio = "/Users/usuario/pasta"; // Substitua pelo diretório desejado
    if (filesystem::exists(diretorio)) {
        cout << "O diretório existe!" << endl;
    } else {
        cout << "O diretório não existe!" << endl;
    }
    return 0;
}
```

Saída:
```
O diretório existe!
```

## Mergulho profundo:

### Contexto histórico:
A verificação da existência de diretórios tem sido uma tarefa importante para a programação desde os primórdios do C++. Em versões anteriores da linguagem, os programadores precisavam usar funções personalizadas para realizar essa tarefa.

### Alternativas:
Além da função `std::filesystem::exists()`, também é possível utilizar a função `std::experimental::filesystem::exists()` ou as funções personalizadas `opendir()` e `closedir()`.

### Detalhes de implementação:
A função `std::filesystem::exists()` faz parte da biblioteca `<filesystem>` introduzida no C++17. Para usá-la, é necessário adicionar a opção `-std=c++17` ao compilar o programa. Além disso, também é necessário incluir a diretiva `using namespace std::filesystem` antes do uso da função.

## Veja também:

- [Documentação da função `std::filesystem::exists()`](https://en.cppreference.com/w/cpp/filesystem/exists)
- [Exemplo completo de como verificar a existência de um diretório em C++](https://www.geeksforgeeks.org/how-to-check-if-directory-exists-in-c-language/)
- [Outras funções úteis da biblioteca `<filesystem>`](https://www.techiedelight.com/filesystem-library-cpp17/)