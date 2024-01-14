---
title:                "Arduino: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que comparar datas é importante para programação Arduino?

Comparar datas é útil em muitos projetos de programação Arduino, especialmente aqueles que envolvem agendamento de tarefas ou monitoramento de intervalos de tempo.

## Como fazer comparações de datas em Arduino

Para comparar datas em Arduino, primeiro precisamos armazenar as datas em variáveis usando a estrutura de dados "struct". Por exemplo, podemos ter duas datas armazenadas como "struct data inicial" e "struct data final". Em seguida, podemos usar a função "time()" para obter o número total de segundos desde 1 de janeiro de 1970. Finalmente, podemos utilizar operações matemáticas simples para comparar esses valores de tempo e determinar qual data ocorre primeiro ou último.

```Arduino
// Armazenando as datas em variáveis
struct data_inicial {
    int dia;
    int mes;
    int ano;
};

struct data_final {
    int dia;
    int mes;
    int ano;
};

// Obter o número total de segundos desde 1 de janeiro de 1970
long segundos_inicial = time(&data_inicial);
long segundos_final = time(&data_final);

// Comparando os valores de tempo
if (segundos_inicial > segundos_final) {
    // A data inicial ocorre após a data final
} else if (segundos_inicial == segundos_final) {
    // As datas são iguais
} else {
    // A data final ocorre após a data inicial
}

``` 

## Profundando na comparação de datas

Uma coisa importante a ter em mente ao comparar datas em Arduino é que o tempo é medido em segundos, então precisamos converter nossas datas em segundos antes de poder compará-las. Além disso, é importante considerar fatores como fuso horário e dias bissextos ao trabalhar com datas em projetos de Arduino.

## Veja também

- [Tutorial sobre structs em Arduino](https://www.arduino.cc/en/Reference/Struct)
- [Documentação sobre a função "time()" em Arduino](https://www.arduino.cc/reference/en/language/functions/time/time/)
- [Dicas para trabalhar com datas em projetos de Arduino](https://www.allaboutcircuits.com/projects/arduino-timing-methods-and-millis-vs-micros-vs-millisec/)

_Obrigado por ler e compartilhar suas experiências com comparações de datas em projetos de Arduino!_