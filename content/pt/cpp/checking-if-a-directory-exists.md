---
title:                "C++: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que verificar se um diretório existe?

Verificar se um diretório existe é uma tarefa importante ao desenvolver programas em C++. Isso pode garantir que o código execute com segurança, evitando erros causados ​​pela tentativa de acessar um diretório inexistente. Além disso, essa verificação pode permitir que o programa tome decisões diferentes, dependendo se o diretório já existe ou não.

## Como fazer?

```C++
#include <iostream>
#include <filesystem> //lib para trabalhar com arquivos e diretórios
namespace fs = std::filesystem;

int main() {
    //verificar se o diretório "teste" existe
    if (fs::exists("teste")) {
        std::cout << "O diretório já existe!";
    }
    else {
        std::cout << "O diretório não existe!";
    }
    return 0;
}
```
Saída:
```
O diretório não existe!
```

## Análise detalhada
Ao verificar se um diretório existe, o programa utiliza a biblioteca `<filesystem>` para acessar sua função `exists()`. Esta função retorna um valor booleano, `true` se o diretório já existe, `false` caso contrário.

Caso o diretório exista, podemos realizar operações como leitura, escrita ou exclusão de arquivos dentro dele. Caso contrário, podemos tomar ações para criar o diretório ou alertar o usuário sobre o erro.

Além disso, é importante lembrar que essa verificação também pode ser feita para arquivos, utilizando a função `is_regular_file()`.

## Veja também
- [Documentação da biblioteca <filesystem> no cplusplus.com](http://www.cplusplus.com/reference/filesystem/)
- [Tutorial sobre como trabalhar com arquivos e diretórios em C++](https://www.geeksforgeeks.org/file-management-c-creating-new-file-name-extension/)
- [Outras funções úteis da biblioteca <filesystem>](https://docs.microsoft.com/en-us/cpp/standard-library/filesystem-filesytem-header?view=msvc-160)