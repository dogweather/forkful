---
title:    "C++: Verificando se um diretório existe"
keywords: ["C++"]
---

{{< edit_this_page >}}

# Por que: Verificando se um diretório existe

Verificar se um diretório existe pode ser uma tarefa essencial em vários projetos de programação C++. Saber se um diretório existe antes de tentar acessá-lo ou manipular seus arquivos pode evitar erros e bugs em seu código.

# Como fazer: Exemplos de código para verificar um diretório existente

Para verificar se um diretório existe em seu código C++, você pode usar a função "opendir()" da biblioteca <dirent.h>. Vamos dar uma olhada em um exemplo básico:

```C++
#include <iostream>
#include <dirent.h>

using namespace std;

int main()
{
    dirent *directory;
    DIR *folder = opendir("caminho/para/o/diretório"); //Substitua pelo caminho do diretório que deseja verificar
    
    if (folder)
    {
        while ((directory = readdir(folder)) != NULL) // Loop para percorrer todos os arquivos do diretório
        {
           cout << directory->d_name << endl; //Imprime o nome dos arquivos 
        }
        
        closedir(folder); //Fecha o diretório
    }

    return 0;
}
```
Ao executar este código, se o diretório existir, você verá uma lista com os nomes de todos os arquivos presentes no diretório. Caso contrário, não haverá saída, indicando que o diretório não existe.

Outra forma de verificar a existência de um diretório é usando a função "std::filesystem::is_directory()" da biblioteca <filesystem> do C++17. Vamos dar uma olhada em um exemplo com esta função:

```C++
#include <iostream>
#include <filesystem>

using namespace std;

int main()
{
    string path = "caminho/para/o/diretório"; //Substitua pelo caminho do diretório que deseja verificar
    
    if (filesystem::is_directory(path))
    {
        cout << "O diretório existe!" << endl;
    }
    else
    {
        cout << "O diretório não existe." << endl;
    }

    return 0;
}
```
Neste exemplo, a função "is_directory()" retorna um valor booleano, indicando se o diretório existe ou não. Se o diretório existir, a mensagem "O diretório existe!" será impressa no console. Se não existir, a mensagem "O diretório não existe." será impressa.

# Mergulho profundo: Mais informações sobre a verificação de diretórios

Em ambos os exemplos acima, é importante notar que os caminhos para os diretórios devem ser passados corretamente para que a verificação funcione corretamente. Além disso, é importante lembrar que a função "opendir()" só funciona com diretórios no sistema de arquivos atual, enquanto a função "is_directory()" pode ser usada com caminhos de rede.

Além disso, também é possível verificar a existência de um diretório usando a função "access()" da biblioteca <unistd.h>. No entanto, esta função só funciona no sistema operacional Unix e pode ser menos eficiente do que as outras opções.

# Ver também

- Documentação da função "opendir()" - https://www.ibm.com/support/knowledgecenter/ssw_ibm_i_73/rtref/opendir.htm
- Documentação da função "is_directory()" - https://en.cppreference.com/w/cpp/filesystem/is_directory
- Documentação da função "access()" - https://www.gnu.org/software/libc/manual/html_node/Testing-File-Access.html