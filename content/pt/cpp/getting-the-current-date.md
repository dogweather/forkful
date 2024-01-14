---
title:    "C++: Obtendo a data atual"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Por que obter a data atual?

Existem muitas razões pelas quais você pode precisar obter a data atual em seu programa C++. Pode ser para fins de registro, para apresentar informações precisas para o usuário ou para realizar cálculos baseados na data. Independentemente da finalidade, ter a capacidade de obter a data atual é uma habilidade básica importante que todo programador deve dominar.

## Como obter a data atual

Em C++, existem várias maneiras de obter a data atual. A seguir está um exemplo simples de como fazer isso usando a biblioteca `ctime`:

```C++
#include <iostream>
#include <ctime>

using namespace std;

int main() {
    // Obter a data atual
    time_t currentTime = time(0);

    // Converter a data para uma string legível
    char* currentDate = ctime(&currentTime);

    // Imprimir a data atual
    cout << "A data atual é: " << currentDate << endl;

    return 0;
}
```

Saída:

```
A data atual é: Sat Feb 27 11:07:09 2021
```

Este é apenas um exemplo simples, mas você também pode formatar a data usando a função `strftime()` e personalizá-la de acordo com suas necessidades. Certifique-se de consultar a documentação da biblioteca `ctime` para obter mais informações sobre todas as funções disponíveis para trabalhar com datas e horas.

## Mergulho mais profundo

Há muitos detalhes interessantes sobre como o C++ obtém a data e hora atuais. Internamente, a biblioteca `ctime` usa uma função chamada `gettimeofday()` para obter a data atual a partir do sistema operacional. Esta função retorna um valor de tempo que é o número de segundos passados desde 1 de janeiro de 1970, também conhecido como "tempo Unix". Essa medida é comumente usada em sistemas operacionais baseados em Unix, incluindo Linux e macOS. No entanto, se você estiver trabalhando em um sistema operacional diferente, pode ser usado um método diferente para obter a data atual.

Existem também outras bibliotecas, como `chrono` e `boost`, que fornecem maneiras mais avançadas de trabalhar com datas e horas em C++.

## Veja também

- [Documentação da biblioteca ctime](https://www.cplusplus.com/reference/ctime/)
- [Exemplos de formatação de data em C++](https://www.w3schools.com/cpp/cpp_date.asp)
- [Como usar a biblioteca chrono em C++](https://www.learncpp.com/cpp-tutorial/8-12-stdchrono/)
- [Biblioteca boost para manipulação de datas e horas em C++](https://www.boost.org/doc/libs/1_75_0/doc/html/date_time.html)