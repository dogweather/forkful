---
title:                "C++: Convertendo uma string para minúsculas"
simple_title:         "Convertendo uma string para minúsculas"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por que
Se você está trabalhando em um projeto de programação em C++ e se deparou com a necessidade de converter uma string para letras minúsculas, este post é para você. A conversão de uma string para lower case é uma tarefa comum em muitos projetos e é importante saber como fazê-la de forma correta.

## Como fazer
Aqui, vamos mostrar como converter uma string para letras minúsculas em C++, usando a função `tolower()` da biblioteca padrão `<cctype>`. Este método funciona para caracteres ASCII, por isso tenha isso em mente caso esteja usando caracteres de outros idiomas.

```
#include <iostream>
#include <cctype> // incluindo biblioteca para usar a função tolower()
using namespace std;

int main() {
    string texto = "Ola Mundo!";

    for (char& c : texto) { // percorrendo a string
        c = tolower(c); // convertendo cada caractere para minúsculo
    }

    cout << texto << endl; // imprimindo a string convertida
    return 0;
}

```

Output:
```
ola mundo!
```

Você também pode usar a função `transform()` da biblioteca `<algorithm>` para converter uma string inteira para minúsculo de uma só vez:

```
#include <iostream>
#include <algorithm> // incluindo biblioteca para usar a função transform()
using namespace std;

int main() {
    string texto = "Ola Mundo!";

    transform(texto.begin(), texto.end(), texto.begin(), ::tolower); // convertendo a string para lower case

    cout << texto << endl; // imprimindo a string convertida
    return 0;
}

```

Output:
```
ola mundo!
```

## Aprofundando
Há algumas coisas a se ter em mente ao converter uma string para minúsculo em C++. A primeira é que o método apresentado acima só funciona para caracteres ASCII, então se você estiver trabalhando com caracteres de outros idiomas, pode ser necessário usar outras funções ou bibliotecas específicas para garantir a conversão correta.

Além disso, lembre-se de estar atento à codificação utilizada em seu programa. Se sua string estiver em uma codificação diferente da ASCII, a conversão para lower case pode não funcionar corretamente.

E por fim, caso esteja trabalhando com strings longas ou em um algoritmo que precise converter muitas strings para lower case, pode ser mais eficiente utilizar outras técnicas mais otimizadas. Portanto, sempre esteja ciente do contexto em que está aplicando a conversão e adapte seu código de acordo.

## Veja também
- [Tabela ASCII](https://pt.wikipedia.org/wiki/ASCII)
- [Função `tolower()` - cplusplus.com](http://www.cplusplus.com/reference/cctype/tolower/)
- [Função `transform()` - cplusplus.com](http://www.cplusplus.com/reference/algorithm/transform/)