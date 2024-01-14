---
title:                "C++: Verificando se um diretório existe"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Porquê

Verificar se um diretório existe é uma tarefa comum em muitos projetos de programação. Saber como fazer isso pode economizar tempo e ajudar a evitar erros no código.

## Como Fazer

Para verificar se um diretório existe em C++, podemos usar a função `opendir()` da biblioteca `<dirent.h>`. Veja o exemplo abaixo:

```C++
#include <iostream>
#include <dirent.h>

using namespace std;

int main() {
    // diretório a ser verificado
    string dir_path = "C:/Users/Utilizador/Desktop/Meu_Diretorio";

    // tentar abrir o diretório
    DIR* dir = opendir(dir_path.c_str());

    // checar se o diretório existe
    if (dir) {
        cout << dir_path << " existe!" << endl;
        closedir(dir);
    } else {
        cout << dir_path << " não existe!" << endl;
    }

    return 0;
}
```
Output:
```
C:/Users/Utilizador/Desktop/Meu_Diretorio existe!
```

No exemplo acima, usamos a função `opendir()` para tentar abrir o diretório especificado na variável `dir_path`. Se a função for bem-sucedida, significa que o diretório existe e, portanto, imprimimos a mensagem correspondente. Caso contrário, imprime-se a mensagem de que o diretório não existe.

## Deep Dive

Para entendermos melhor como essa verificação funciona, é importante entender a função `opendir()`. Esta função retorna um ponteiro para uma estrutura `DIR` que contém informações sobre o diretório. Se o ponteiro for igual a `NULL`, significa que a função falhou em sua tarefa de abrir o diretório, indicando que ele não existe.

Outra maneira de verificar se um diretório existe é usando a função `ifstream` da biblioteca `<fstream>`. No entanto, ao contrário da função `opendir()`, essa função só pode verificar se um arquivo existe, não um diretório. 

## Veja Também

- <https://www.cplusplus.com/reference/cstdio/opendir/>
- <https://docs.microsoft.com/pt-br/cpp/c-runtime-library/reference/opendir-wfd?view=msvc-160>