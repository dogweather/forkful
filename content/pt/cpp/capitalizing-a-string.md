---
title:    "C++: Capitalizando uma string"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Por que Capitalizar uma String?

Capitalizar uma string é um processo comum em programação que consiste em transformar todas as letras de uma palavra ou frase em letras maiúsculas. Isso pode ser necessário para padronizar a formatação de dados ou para facilitar a comparação de strings.

## Como Fazer

Para capitalizar uma string em C++, existe uma função chamada `toupper()` que pode ser utilizada. Ela pertence à biblioteca `cctype` e recebe como parâmetro um caractere que será convertido em maiúsculo.

Para aplicar essa função em uma string, é necessário percorrer todos os caracteres da string e aplicar a função a cada um deles. Isso pode ser feito de forma manual, mas também existem funções específicas para isso, como a função `transform()` da biblioteca `algorithm`.

Veja um exemplo de código em C++ abaixo:

```C++
#include <iostream>
#include <cctype>
#include <string>
#include <algorithm>

using namespace std;

int main() {
   string minhaString = "exemplo de string";

   // percorrendo manualmente
   for (int i = 0; i < minhaString.length(); i++) {
      minhaString[i] = toupper( minhaString[i] );
   }

   cout << "String capitalizada: " << minhaString << endl;

   // utilizando transform
   transform(minhaString.begin(), minhaString.end(), minhaString.begin(), ::toupper);

   cout << "String capitalizada: " << minhaString << endl;

   return 0;
}
```

A saída desse código será:

```
String capitalizada: EXEMPLO DE STRING
String capitalizada: EXEMPLO DE STRING
```

## Uma Análise Mais Profunda

Além da função `toupper()`, existem outras formas de capitalizar uma string em C++. Uma delas é utilizando a função `toupper()` em conjunto com a função `ispunct()` para checar se um caractere é uma pontuação. Isso é importante porque nem sempre queremos converter pontuações para maiúsculas.

Outra forma é utilizando o operador de decremento `--` em conjunto com a função `getline()` para converter a primeira letra de cada palavra em maiúscula.

Essas são apenas algumas possibilidades de capitalização de strings em C++. É importante entender o algoritmo por trás do processo e pensar em possíveis cenários para escolher a melhor abordagem.

## Veja também

- [Strings em C++](https://www.cplusplus.com/reference/string/string/)
- [Função transform em C++](https://www.cplusplus.com/reference/algorithm/transform/)
- [Função toupper em C++](https://www.cplusplus.com/reference/cctype/toupper/)
- [Função ispunct em C++](https://www.cplusplus.com/reference/cctype/ispunct/)