---
title:    "C++: Encontrando o tamanho de uma string."
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por que?

Encontrar o comprimento de uma string é uma tarefa comum na programação. Saber a quantidade de caracteres em uma string é importante para várias operações, como limitar a entrada de dados, criar espaçamento em um texto e muito mais.

## Como Fazer

Para encontrar o comprimento de uma string em C++, podemos utilizar a função `strlen()` da biblioteca `string.h`. Ela retorna um valor do tipo `size_t`, que representa o tamanho da string.

```C++
#include <iostream>
#include <string.h>

using namespace std;

int main()
{
    char palavra[50];
    cout << "Digite uma palavra: ";
    cin >> palavra;

    size_t tamanho = strlen(palavra);
    cout << "O tamanho da palavra " << palavra << " e: " << tamanho << endl;

    return 0;
}
```

**Saída:**

```
Digite uma palavra: programação
O tamanho da palavra programação é: 11
```

Podemos também utilizar a função `length()` do objeto `string` da biblioteca `string`, que retorna um valor do tipo `size_t`.

```C++
#include <iostream>
#include <string>

using namespace std;

int main()
{
    string frase;
    cout << "Digite uma frase: ";
    getline(cin, frase);

    size_t tamanho = frase.length();
    cout << "O tamanho da frase '" << frase << "' e: " << tamanho << endl;

    return 0;
}
```

**Saída:**

```
Digite uma frase: Amanhã é sexta-feira!
O tamanho da frase 'Amanhã é sexta-feira!' é: 21
```

## Aprofundando-se

Além das funções mencionadas acima, também é possível utilizar outras técnicas para encontrar o comprimento de uma string em C++. Por exemplo, podemos percorrer a string utilizando um loop `for` e contar a quantidade de caracteres até encontrar o caractere nulo `'\0'` que marca o final da string.

```C++
#include <iostream>

using namespace std;

int main()
{
    char frase[50];
    cout << "Digite uma frase: ";
    cin.getline(frase, 50);

    int tamanho = 0;
    for (int i = 0; frase[i] != '\0'; i++)
    {
        tamanho++;
    }
    cout << "O tamanho da frase '" << frase << "' e: " << tamanho << endl;

    return 0;
}
```

**Saída:**

```
Digite uma frase: Tenho que terminar esse projeto hoje.
O tamanho da frase 'Tenho que terminar esse projeto hoje.' é: 40
```

Lembre-se de sempre tratar strings com cuidado, pois elas podem ser sensíveis a maiúsculas e minúsculas e também podem conter acentos e caracteres especiais, o que pode afetar o resultado das suas operações.

## Veja também

- [Documentação oficial do C++ sobre a função `strlen()`](https://en.cppreference.com/w/cpp/string/byte/strlen)
- [Documentação oficial do C++ sobre a função `length()`](https://en.cppreference.com/w/cpp/string/basic_string/length)