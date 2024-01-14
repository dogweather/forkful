---
title:                "C++: Capitalizando uma string"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por que capitalizar uma string em C++

Ao escrever código em C++, é comum nos depararmos com a necessidade de manipular strings. Algumas vezes, precisamos garantir que todas as letras em uma string estejam maiúsculas, o que pode ser feito por meio da capitalização. Neste artigo, vamos explorar como podemos capitalizar uma string em C++.

## Como fazer a capitalização de uma string em C++

Existem diferentes formas de capitalizar uma string em C++, mas vamos focar na mais simples e eficiente - usando a função `toupper()` da biblioteca `cctype`.

Primeiramente, devemos garantir que a biblioteca `cctype` esteja incluída em nosso código:

```C++
#include <cctype>
```
Em seguida, definimos a string que desejamos capitalizar:

```C++
string palavra = "exemplo";
```

Para capitalizar a string, usamos um loop `for` que percorre cada caractere da string e utiliza a função `toupper()` para transformá-lo em maiúsculo:

```C++
for (int i = 0; i < palavra.length(); i++) {
  palavra[i] = toupper(palavra[i]);
}
```

Por fim, podemos imprimir a string capitalizada:

```C++
cout << palavra << endl;
```

O output será:

```C++
EXEMPLO
```

## Aprofundando na capitalização de strings em C++

É importante lembrar que essa é uma forma básica de capitalizar uma string em C++ e pode não funcionar em todas as situações. Por exemplo, se a string contiver caracteres acentuados, eles não serão capitalizados corretamente.

Além disso, essa abordagem altera a string original, o que pode ser indesejado em certas situações. Para evitar isso, podemos criar uma cópia da string original e capitalizar apenas a cópia.

Outra abordagem é criar uma função que verifique se cada caractere é uma letra e, caso seja, faça a capitalização. Isso pode ser útil quando precisamos capitalizar apenas uma parte da string, não a string inteira.

## Veja também
- [Funções de manipulação de strings em C++](https://www.cplusplus.com/reference/string/string/)
- [Documentação da função `toupper()`](https://www.cplusplus.com/reference/cctype/toupper/)
- [Exemplos de capitalização de strings em C++](https://www.programiz.com/cpp-programming/library-function/cctype/toupper)