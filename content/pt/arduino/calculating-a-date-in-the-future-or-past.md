---
title:                "Calculando uma data no futuro ou passado"
html_title:           "Arduino: Calculando uma data no futuro ou passado"
simple_title:         "Calculando uma data no futuro ou passado"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## O que e por quê?

Calcular uma data no futuro ou no passado é um processo comumente usado por programadores para realizar tarefas como programação de alarmes ou temporizadores. Essencialmente, é uma forma de usar o tempo e a data do mundo real em nossos códigos e tornar nossos dispositivos mais interativos e funcionais.

## Como fazer:

```
Arduino nano;
int dia = 15; //dia desejado
int mes = 6; //mês desejado
int ano = 2021; //ano desejado
int hora = 13; //hora desejada
int minutos = 30; //minutos desejados
int segundos = 0; //segundos desejados

void setup() {
    Serial.begin(9600);
    pinMode(2, OUTPUT); //pino para controlar o LED
}

void loop() {
    //definir a data atual
    const unsigned long dataAtual = atualData();
    
    //definir data desejada
    const unsigned long dataDesejada = dateToLong(dia, mes, ano, hora, minutos, segundos);
    
    //calcular diferença de tempo
    unsigned long diferenca = dataDesejada - dataAtual;
    
    //converter diferença para dias, horas e minutos
    int dias = (int)diferenca / (24*60*60);
    int horas = (int)diferenca / (60*60) % 24;
    int minutos = (int)diferenca / 60 % 60;
    
    //imprimir resultado
    Serial.println("Faltam " + String(dias) + " dias, " + String(horas) + " horas e " + String(minutos) + " minutos para chegar à data desejada!");
    
    //liga ou desliga o LED se a data desejada for no dia de hoje
    if(diferenca == 0) {
        digitalWrite(2, HIGH);
    }
    else {
        digitalWrite(2, LOW);
    }
    
    delay(5000);
}

//função para obter a data atual
unsigned long atualData() {
    return ((unsigned long)day() * 24*60*60 + (unsigned long)month() * 60*60 + (unsigned long)year() * 60);
}

//função para converter data para um formato legível
unsigned long dateToLong(int dia, int mes, int ano, int hora, int minutos, int segundos) {
    return ((unsigned long)dia * 24*60*60 + (unsigned long)mes * 60*60 + (unsigned long)ano * 60 + (unsigned long)hora * 60 + (unsigned long)minutos * 60 + (unsigned long)segundos);
}
```

## Deep Dive:

Calcular datas no futuro ou no passado é um recurso amplamente usado em diferentes áreas da programação, como automação residencial, projetos de IoT, jogos e muito mais. Ele nos permite criar dispositivos que respondem a diferentes eventos baseados no tempo e torna nossos códigos mais precisos e interativos. Existem também algumas bibliotecas disponíveis para facilitar o cálculo de datas no Arduino, como a biblioteca Time.h.

## Veja também:

- [Página oficial do Arduino](https://www.arduino.cc)
- [Documentação da biblioteca Time.h](http://playground.arduino.cc/Code/Time)
- [Arduino para iniciantes: programando datas e horas](https://www.filipeflop.com/blog/arduino-para-iniciantes-programando-datas-e-horas/)