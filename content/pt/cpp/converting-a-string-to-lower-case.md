---
title:    "C++: Transformando uma string em letras minúsculas"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Por que
Em programação, muitas vezes precisamos manipular strings em nossos códigos. Uma das tarefas mais comuns é converter uma string para letras minúsculas. Isso pode ser útil, por exemplo, ao comparar duas strings ou ao validar a entrada do usuário. Neste artigo, vamos explorar como converter uma string para letras minúsculas em C++.

## Como fazer
Para converter uma string para letras minúsculas em C++, podemos utilizar a função `tolower` da biblioteca `<cctype>`. Ela recebe como parâmetro um caractere e retorna o seu equivalente em letra minúscula. Podemos então percorrer a string, caractere por caractere, e aplicar essa função a cada um deles. Veja um exemplo de código:

```C++
#include <iostream>
#include <cctype>
#include <string>

using namespace std;

string converterParaMinusculas(string str) {
  string resultado;
  for (char c : str) {
    resultado += tolower(c);
  }
  return resultado;
}

int main() {
  string minhaString = "PROGRAMANDO EM C++";
  cout << converterParaMinusculas(minhaString) << endl; // saída: programando em c++
  return 0;
}
```

Neste código, criamos uma função `converterParaMinusculas` que recebe uma string como parâmetro e retorna a mesma string com todas as letras minúsculas. Em seguida, na função `main`, criamos uma string com letras maiúsculas e usamos a função criada para convertê-la para letras minúsculas. Finalmente, imprimimos o resultado na tela.

## Explorando mais a fundo
Ao trabalhar com a função `tolower`, é importante lembrar que ela só funciona com caracteres individuais. Portanto, se a sua string contiver caracteres especiais, a conversão pode não ser feita corretamente. Além disso, ela só funciona com a tabela ASCII, então pode não funcionar com caracteres de outros idiomas.

Uma alternativa é utilizar funções específicas para cada idioma, como a `tolower` da biblioteca `<locale>`, que tem suporte para caracteres unicode. Ou ainda, utilizar bibliotecas externas que fazem a conversão de forma mais abrangente.

É importante também lembrar que a função `tolower` não altera a string original, apenas retorna uma string com as letras minúsculas. Portanto, é necessário atribuir o resultado a uma nova variável ou reatribuir à mesma variável, como fizemos no exemplo anterior.

## Veja também
- [Tutorial de strings em C++](https://www.cplusplus.com/doc/tutorial/strings/)
- [Documentação da função `tolower`](http://www.cplusplus.com/reference/cctype/tolower/)
- [Biblioteca de função `tolower` da `<locale>`](http://www.cplusplus.com/reference/locale/tolower/)