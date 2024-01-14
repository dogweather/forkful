---
title:    "C++: Calculando uma data no futuro ou passado"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Por que

Calcular a data no futuro ou no passado pode ser útil em diversas situações, como planejar viagens ou eventos, gerenciar prazos de projetos ou até mesmo verificar datas de validade de produtos.

## Como Fazer

Para calcular uma data no futuro ou no passado, é necessário primeiro definir uma data base, ou seja, a data atual a partir da qual o cálculo será feito. Em seguida, deve-se determinar a quantidade de dias, meses ou anos que deseja adicionar ou subtrair da data base. Veja um exemplo de código utilizando a linguagem C++:

```C++
#include <iostream>
#include <ctime>

using namespace std;

int main() {
    // Definindo data base como 10/05/2020
    time_t dataBase = 1589085600; 
    
    // Adicionando 10 dias à data base
    dataBase += 10 * 86400; // 1 dia = 86400 segundos
    
    // Convertendo para struct tm (formato de data utilizado pela linguagem C++)
    tm *dataFutura = localtime(&dataBase);
    
    // Imprimindo a data futura no formato dd/mm/aaaa
    cout << dataFutura->tm_mday << "/" << 1 + dataFutura->tm_mon << "/" << 1900 + dataFutura->tm_year << endl;
    
    return 0;
}
```

A saída deste código seria: 20/05/2020.

## Deep Dive

Para aqueles que desejam se aprofundar neste assunto, é importante entender que o cálculo de datas é baseado no sistema de tempo Unix. Nesse sistema, o tempo é contado a partir de 1 de janeiro de 1970, às 00:00:00 em UTC (Coordinated Universal Time). Ou seja, cada segundo é contado a partir dessa data base, o que facilita o cálculo de datas no futuro ou no passado.

Além disso, é importante estar atento aos fatores que podem influenciar no resultado final do cálculo, como o fato de alguns meses terem um número diferente de dias (fevereiro tem 28 ou 29 dias, dependendo do ano) e a existência de anos bissextos.

## Veja também

- [Tutorial de C++ no W3Schools](https://www.w3schools.com/cpp/)
- [Documentação sobre datas em C++](https://en.cppreference.com/w/cpp/chrono)
- [Como calcular datas no Excel](https://support.microsoft.com/pt-br/office/calcular-diferença-entre-datas-e-horas-em-excel-25aecfcd-2414-4268-9336-b27fd6fa05ab)