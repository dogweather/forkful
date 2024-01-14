---
title:    "C++: Verificando se um diretório existe"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que 

Verificar se um diretório existe é uma tarefa importante em programação, pois garante que o programa possa acessar os arquivos e pastas necessários para sua execução. Além disso, essa verificação também pode ajudar a evitar erros e bugs no código.

## Como fazer

Para verificar se um diretório existe em C++, podemos usar a função `opendir()` da biblioteca `<dirent.h>`, que abre um diretório e retorna um ponteiro para ele. Em seguida, podemos usar a função `closedir()` para fechá-lo. Veja um exemplo de código abaixo:

```C++
#include <iostream>
#include <dirent.h>

int main() {
    // diretório a ser verificado
    const char* path = "/home/user/Downloads";

    // abrindo o diretório
    DIR* dir = opendir(path);

    if (dir) {
        std::cout << "Diretório existe.";
        closedir(dir);
    } else {
        std::cout << "Diretório não existe.";
    }

    return 0;
}
```

Neste exemplo, usamos a função `opendir()` para abrir o diretório "/home/user/Downloads" e armazenamos o seu ponteiro em `dir`. Em seguida, verificamos se esse ponteiro é válido, indicando que o diretório existe. Caso contrário, o diretório não existe. Por fim, usamos a função `closedir()` para fechar o diretório.

A saída deste código seria: "Diretório existe", caso o diretório exista, ou "Diretório não existe", caso contrário.

## Mergulho profundo

Uma maneira de verificar se um diretório existe em um nível mais profundo seria usar a função `stat()` da biblioteca `<sys/stat.h>`. Esta função retorna informações sobre um determinado arquivo ou diretório, como seu tamanho, permissão e data de modificação. Veja um exemplo de código usando esta função:

```C++
#include <iostream>
#include <sys/stat.h>

int main() {
    // diretório a ser verificado
    const char* path = "/home/user/Downloads";

    // verificando informações sobre o diretório
    struct stat info;
    if (stat(path, &info) == 0 && S_ISDIR(info.st_mode)) {
        std::cout << "Diretório existe.";
    } else {
        std::cout << "Diretório não existe.";
    }

    return 0;
}
```

No código acima, usamos a função `stat()` para obter informações sobre o diretório "/home/user/Downloads" e armazená-las na estrutura `info`. Em seguida, verificamos se o modo de arquivo (`st_mode`) é um diretório (`S_ISDIR`) antes de imprimir a mensagem adequada.

## Veja também

- [Documentação da função opendir()](https://www.man7.org/linux/man-pages/man3/opendir.3.html)
- [Documentação da função closedir()](https://www.man7.org/linux/man-pages/man3/closedir.3.html)
- [Documentação da função stat()](https://www.man7.org/linux/man-pages/man2/stat.2.html)