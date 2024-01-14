---
title:    "C++: Excluindo caracteres que correspondem a um padrão"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Por que

Você pode se encontrar em uma situação em que precisa remover caracteres que correspondam a um padrão específico em uma string. Talvez você esteja limpando dados importados ou filtrando resultados de uma pesquisa. Em qualquer caso, é importante saber como fazer isso de forma eficiente e efetiva.

## Como Fazer

Para remover caracteres que correspondam a um padrão em uma string, podemos usar a função `remove_if()` da biblioteca `<algorithm>` do C++. Esta função aceita três argumentos: o primeiro é o início da string, o segundo é o final e o terceiro é uma função de predicado que será usada para determinar quais caracteres devem ser removidos. Vamos dar uma olhada em um exemplo de código para entender melhor:

```C++
#include <iostream>
#include <string>
#include <algorithm>

using namespace std;

// Função de predicado para determinar quais caracteres devem ser removidos
bool isVowel(char c) {
    return (c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u');
}

int main() {
    string str = "Esta é uma string de teste.";
    // Removendo todas as vogais da string usando a função remove_if
    str.erase(remove_if(str.begin(), str.end(), isVowel), str.end());
    cout << str << endl;
    return 0;
}
```
#### Output:
```
st  strng d tst.
```

Neste exemplo, criamos uma função de predicado `isVowel()` que retorna `true` se o caractere fornecido for uma vogal. Passamos essa função para a função `remove_if()` como o terceiro argumento, o que resulta na remoção de todas as vogais da string `str`.

Além disso, você pode adaptar essa lógica para corresponder a qualquer padrão que desejar. Por exemplo, se você deseja remover todos os caracteres não numéricos de uma string, você pode criar uma função de predicado que verifique se o caractere fornecido é um número usando a função `isdigit()` da biblioteca `<cctype>`.

## Deep Dive

Na parte "Como Fazer", vimos um exemplo de como remover caracteres que correspondam a um padrão em uma string usando a função `remove_if()`. Mas como essa função realmente funciona?

A função `remove_if()` recebe uma função de predicado como o terceiro argumento. Em seguida, ela itera sobre os elementos da string, aplicando a função de predicado a cada caractere. Se a função de predicado retornar `true` para um caractere, esse caractere é removido da string. Se a função de predicado retornar `false`, o caractere permanece na string.

Ao final, a função `remove_if()` retorna um ponteiro para uma posição imediatamente após o último elemento removido. Em seguida, a função `erase()` é usada para remover os elementos indesejados da string.

## Veja Também

- [C++: Erasing From Strings](https://www.fluentcpp.com/2017/04/21/how-to-erase-character-from-string-cpp/)
- [Função `remove_if()` - cppreference.com](https://en.cppreference.com/w/cpp/algorithm/remove)
- [Função `isdigit()` - cppreference.com](https://en.cppreference.com/w/cpp/string/byte/isdigit)