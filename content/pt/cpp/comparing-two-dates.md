---
title:    "C++: Comparando duas datas"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que comparar datas em um programa C++?

Comparar datas é uma tarefa comum em programas de computador, especialmente em aplicativos relacionados a calendários, cronogramas ou eventos. Ao comparar duas datas, podemos determinar qual é a mais recente, qual é a mais antiga ou se elas são iguais. Isso é importante para garantir que nosso programa funcione corretamente e gere resultados precisos.

## Como comparar duas datas em C++

Para comparar duas datas em um programa C++, precisaremos usar algumas funções e operadores específicos. Vamos supor que temos duas variáveis do tipo `tm struct` que representam datas e queremos compará-las. Podemos fazer isso utilizando o operador de comparação `>` ou `<` para verificar se uma data é maior ou menor do que a outra.

```C++
// Declaração das variáveis de data
tm data1, data2;

// Atribuição de valores às variáveis
data1.tm_mday = 15;
data1.tm_mon = 5;
data1.tm_year = 2021;

data2.tm_mday = 10;
data2.tm_mon = 5;
data2.tm_year = 2021;

// Comparando as datas
if (data1 > data2) {
    // Data1 é mais recente que Data2
    cout << "Data1 é mais recente que Data2";
}
else if (data1 < data2) {
    // Data1 é mais antiga que Data2
    cout << "Data1 é mais antiga que Data2";
}
else {
    // As datas são iguais
    cout << "Data1 e Data2 são iguais";
}
```
Saída:
```
Data1 é mais recente que Data2
```

## Mais informações sobre a comparação de datas

Além de usar os operadores de comparação `<` e `>`, também podemos usar a função `difftime()` para calcular a diferença entre duas datas em segundos. Essa função leva em consideração os anos bissextos e o número de segundos em cada mês.

Podemos também fazer comparações mais precisas, como verificar se uma data é exatamente igual à outra, levando em consideração não apenas o dia, mês e ano, mas também a hora, minuto e segundo. Para isso, podemos usar a função `mktime()` para converter as variáveis `tm struct` em valores do tipo `time_t` e então compará-las usando o operador `==`.

## Veja também

- [Documentação da linguagem C++ sobre a estrutura tm](https://www.cplusplus.com/reference/ctime/tm/)
- [Tutorial sobre manipulação de datas em C++](https://www.geeksforgeeks.org/date-time-manipulations-in-c/)
- [Exemplos de código de comparação de datas em C++](https://www.bitdegree.org/learn/date-and-time-in-c-plus-plus#comparing-time-t-vs-time-t-objects-in-c)