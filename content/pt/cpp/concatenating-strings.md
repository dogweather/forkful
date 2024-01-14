---
title:                "C++: Juntando cadeias de caracteres"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por que

Às vezes, precisamos combinar várias cadeias de caracteres em uma única sequência. Isso pode ser útil para imprimir uma mensagem personalizada, criar uma URL dinâmica ou manipular dados em um arquivo. É aqui que entra em jogo a concatenação de strings em C++. 

## Como fazer

A concatenação de strings em C++ é bastante fácil e pode ser feita de diferentes maneiras, dependendo da versão do C++ que você está usando. Aqui está um exemplo usando a sintaxe do C++11:

```C++
#include <iostream>
#include <string>
using namespace std;

int main()
{
    // Declare duas strings
    string saudacao = "Olá";
    string nome = "Maria";

    // Concatene a saudação e o nome
    string mensagem = saudacao + ", " + nome;

    // Imprima a mensagem resultante
    cout << mensagem << endl;

    return 0;
}

/* Saída:
Olá, Maria
*/
```

Aqui, usamos o operador `+` para concatenar as strings `saudacao` e `nome` e armazenamos o resultado em `mensagem`. No final, imprimimos `mensagem` para mostrar a saudação personalizada.

Além do operador `+`, também é possível usar a função `std::stringstream` para concatenar strings em C++. Aqui está um exemplo:

```C++
#include <iostream>
#include <string>
#include <sstream>
using namespace std;

int main()
{
    // Declare duas strings
    string saudacao = "Oi";
    string nome = "João";

    // Use a função std::stringstream para concatenar
    stringstream ss;
    ss << saudacao << ", " << nome;

    // Imprima a mensagem resultante
    cout << ss.str() << endl;

    return 0;
}

/* Saída:
Oi, João
*/
```

## Mergulho profundo

Na verdade, a concatenação de strings em C++ é mais complexa do que apenas usar o operador `+` ou a função `std::stringstream`. Por baixo dos panos, esses métodos estão criando novas strings em memória para armazenar o resultado concatenado. Isso pode ser ineficiente quando manipulamos grandes quantidades de dados.

Uma abordagem mais eficiente é usar a classe `std::string` e sua função `append()`. Veja um exemplo:

```C++
#include <iostream>
#include <string>
using namespace std;

int main()
{
    // Declare duas strings
    string saudacao = "Oi";
    string nome = "José";

    // Reserve espaço em memória para a string resultante
    string mensagem;
    mensagem.reserve(saudacao.length() + nome.length() + 2); // 2 para os espaços e o caractere de nulo

    // Use a função append() para concatenar
    mensagem.append(saudacao).append(" ").append(nome);

    // Imprima a mensagem resultante
    cout << mensagem << endl;

    return 0;
}

/* Saída:
Oi, José
*/
```

Usando `append()`, podemos economizar memória e tornar o processo de concatenação mais eficiente. No exemplo acima, usamos `reserve()` para pré-alocar espaço suficiente para a string resultante, o que evita realocações desnecessárias de memória.

## Veja também

- [Referência do operador + em C++](https://www.learncpp.com/cpp-tutorial/concatenating-strings-and-character-literals/#:~:text=The%20%2B%20operator%20can%20be,is%20concatenated%20to%20the%20string.)
- [Referência da classe std::string em cppreference](https://en.cppreference.com/w/cpp/string/basic_string)
- [Tutorial de C++ da w3schools](https://www.w3schools.com/cpp) (em português do Brasil)