---
title:                "C++: Capitalizando uma string"
simple_title:         "Capitalizando uma string"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por que capitalizar uma string é importante

Capitalizar uma string é uma tarefa comum em muitos programas de computador. É especialmente útil quando queremos garantir que uma string esteja formatada corretamente para facilitar a leitura e o processamento pelos usuários.

## Como capitalizar uma string em C++

Em C++, há várias maneiras de capitalizar uma string, mas usaremos a função `toupper()` da biblioteca `cctype`. Primeiro, devemos incluir a biblioteca em nosso código:

```C++
#include <cctype>
```

Em seguida, podemos usar a função `toupper()` em um loop `for` para iterar por cada caractere da string:

```C++
for(int i = 0; i < str.length(); i++){
    str[i] = toupper(str[i]);
}
```

O loop itera pelos índices da string `str` e atualiza cada caractere usando a função `toupper()`. Por fim, podemos imprimir a string capitalizada para verificar o resultado:

```C++
std::cout << str;
```

Aqui está um exemplo completo de como capitalizar uma string em C++:

```C++
#include <iostream>
#include <string>
#include <cctype>

int main(){
    std::string str = "exemplo de STRING para capitalizar";
    
    for(int i = 0; i < str.length(); i++){
        str[i] = toupper(str[i]);
    }
    
    std::cout << str;
    
    return 0;
}
```

A saída deste programa seria: "EXEMPLO DE STRING PARA CAPITALIZAR".

## Mais informações sobre capitalização de strings

Existem várias outras maneiras de capitalizar uma string em C++, incluindo o uso da função `strlwr()` para transformar a string em letras minúsculas e depois usar `toupper()` para capitalizar apenas a primeira letra. Também é possível criar uma função personalizada para capitalizar uma string, que pode ser útil para lidar com casos específicos.

Além disso, é importante lembrar que a função `toupper()` só funciona para caracteres ASCII, então, se estivermos lidando com strings que contenham caracteres acentuados ou de outros alfabetos, será necessário usar outras funções ou implementar uma solução mais complexa.

## Veja também

- [Documentação oficial da função `toupper()`](https://en.cppreference.com/w/cpp/string/byte/toupper)
- [Outras maneiras de capitalizar uma string em C++](https://www.geeksforgeeks.org/capitalize-string-in-c/)
- [Manipulação de strings em C++](https://www.cplusplus.com/reference/string/string/)