---
title:                "C++: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que comparar duas datas é importante?

Comparar datas é uma tarefa comum na programação, especialmente no desenvolvimento de programas que envolvem agendamentos ou cálculos de prazos. Além disso, a comparação de datas pode ser usada para verificar a validade de dados, como datas de nascimento ou vencimento de documentos.

## Como comparar duas datas em C++?

Para comparar duas datas em C++, utilizamos os operadores de comparação "menor que" (<) e "maior que" (>), que retornam verdadeiro (true) ou falso (false) dependendo da relação entre as datas.

```C++
// Declaração das variáveis de data
int dia1, dia2, mes1, mes2, ano1, ano2;

// Entrada dos valores das datas
cout << "Digite o dia, mês e ano da primeira data: ";
cin >> dia1 >> mes1 >> ano1;

cout << "Digite o dia, mês e ano da segunda data: ";
cin >> dia2 >> mes2 >> ano2;

// Comparação utilizando os operadores de comparação
if (ano1 < ano2) {
    cout << "A primeira data é anterior à segunda data";
} else if (ano1 > ano2) {
    cout << "A segunda data é anterior à primeira data";
} else {
    if (mes1 < mes2) {
        cout << "A primeira data é anterior à segunda data";
    } else if (mes1 > mes2) {
        cout << "A segunda data é anterior à primeira data";
    } else {
        if (dia1 < dia2) {
            cout << "A primeira data é anterior à segunda data";
        } else if (dia1 > dia2) {
            cout << "A segunda data é anterior à primeira data";
        } else {
            cout << "As datas são iguais";
        }
    }
}
```

### Exemplo de saída:

```
Digite o dia, mês e ano da primeira data: 15 05 1990
Digite o dia, mês e ano da segunda data: 10 04 1990
A primeira data é anterior à segunda data
```

## Explorando mais a comparação de datas

É importante lembrar que a comparação de datas em C++ é feita considerando o valor numérico de cada elemento (dia, mês, ano). Por isso, é necessário ter cuidado ao comparar datas que estejam em formatos diferentes, como por exemplo dia/mês/ano e mês/dia/ano.

Além disso, em C++ também podemos utilizar a biblioteca <ctime> para trabalhar com datas e realizar operações, como adicionar ou subtrair dias de uma data específica.

## Veja também

- [Documentação da biblioteca ctime em C++](https://www.cplusplus.com/reference/ctime/)
- [Tutorial de comparação de datas em C++](https://www.geeksforgeeks.org/compare-two-dates-c-2/)