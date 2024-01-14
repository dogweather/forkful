---
title:    "C: Comparando duas datas"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que comparar duas datas em um programa em C?

Ao desenvolver um programa em C, muitas vezes é necessário trabalhar com datas. Isso pode ser para calcular o tempo decorrido, agendar tarefas ou simplesmente para exibir a data atual. Em alguns casos, também pode ser necessário comparar duas datas para verificar se uma é maior, menor ou igual à outra. Saber como fazer essa comparação é importante para garantir a precisão e funcionalidade do seu programa. Neste artigo, vamos explicar por que é importante comparar duas datas e como fazer isso em C.

## Como comparar duas datas em C

A linguagem C possui uma biblioteca padrão chamada time.h, que contém funções para manipulação e exibição de datas e horas. O primeiro passo para comparar datas em C é obter as datas que serão comparadas. Isso pode ser feito usando as funções time() ou localtime().

```C
time_t t = time(NULL); // Obtém a data atual em forma de segundos desde 1 de janeiro de 1970
struct tm *data_atual = localtime(&t); // Converte a data atual em uma estrutura tm
```

Uma vez que você tenha as duas datas que deseja comparar, você pode usar a função difftime() para calcular a diferença entre elas em segundos. Se a diferença for maior que 0, significa que a primeira data é mais recente que a segunda, se for igual a 0, as datas são iguais e se for menor que 0, a primeira data é mais antiga que a segunda.

```C
time_t t1, t2;
// Código para obter as duas datas
double diferença = difftime(t1, t2);
if(diferença > 0) {
    // Código para data1 > data2
} else if(diferença == 0) {
    // Código para data1 = data2
} else {
    // Código para data1 < data2
}
```

## Uma olhada mais profunda na comparação de datas em C

Ao trabalhar com datas, é importante considerar os diferentes formatos de representação. Por exemplo, enquanto podemos pensar em uma data como "10 de maio de 2020", o computador a vê como "2020-05-10". Isso pode afetar a forma como nós, humanos, comparamos datas e pode causar erros em nosso programa se não levado em consideração. Além disso, é importante verificar se as datas estão no mesmo fuso horário antes de fazer a comparação.

Além disso, a biblioteca time.h possui funções para trabalhar com datas em diferentes formatos, como dias, meses e anos separados, ou em segundos desde 1 de janeiro de 1970. É importante escolher o formato mais adequado para o seu programa e garantir que as datas sejam convertidas corretamente antes de compará-las.

## Veja também

- [Documentação oficial da biblioteca time.h em C](https://en.cppreference.com/w/c/chrono)
- [Tutorial de manipulação de data e hora em C](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [Comparando datas em C utilizando strftime](https://www.tutorialspoint.com/compare-dates-in-c-programming-using-strftime)

Esperamos que agora você tenha uma compreensão melhor de por que e como comparar datas em um programa em C. Lembre-se sempre de considerar os diferentes formatos de datas e converter corretamente antes de fazer a comparação. Isso garantirá a precisão e funcionalidade do seu programa.