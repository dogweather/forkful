---
title:                "Capitalizando uma string"
html_title:           "C++: Capitalizando uma string"
simple_title:         "Capitalizando uma string"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por Que

Você já se deparou com a situação de precisar capitalizar uma string em C++, mas não saber como fazer isso da forma mais eficiente? Neste artigo, vamos explorar o porquê de capitalizar uma string e aprender como fazer isso de forma simples e prática.

## Como Fazer
Capitalizar uma string significa transformar todas as letras de uma palavra em maiúsculas. Para fazer isso em C++, podemos utilizar a função `toupper()` da biblioteca `<ctype.h>`. Veja abaixo um exemplo de como usar essa função em um programa que recebe uma string do usuário e a imprime capitalizada:

```C++
#include <iostream>
#include <ctype.h>

using namespace std;

int main() {

    string palavra;

    // Obtém a palavra do usuário
    cout << "Digite uma palavra: ";
    cin >> palavra;

    // Utiliza um loop para transformar cada letra em maiúscula
    for (int i = 0; i < palavra.length(); i++) {
        palavra[i] = toupper(palavra[i]);
    }

    // Imprime a palavra capitalizada
    cout << "Palavra capitalizada: " << palavra << endl;

    return 0;
}
```

**Exemplo de Saída:**

```
Digite uma palavra: programar
Palavra capitalizada: PROGRAMAR
```

## Detalhando
O processo de capitalizar uma string pode ser feito manualmente, percorrendo cada letra da palavra e transformando-a em maiúscula. No entanto, a função `toupper()` é uma opção mais eficiente e prática. Ela faz parte da biblioteca padrão de C++ e pode ser utilizada facilmente em qualquer programa.

Além disso, é importante lembrar que a função `toupper()` também pode ser utilizada para capitalizar apenas a primeira letra de uma palavra, deixando as demais em minúsculo. Isso pode ser útil em situações específicas, como em nomes próprios.

## Veja Também
- [Documentação da função toupper() em C++](https://www.cplusplus.com/reference/cctype/toupper/)
- [Como inverter uma string em C++](https://www.devmedia.com.br/como-inverter-uma-string-em-c/18986)