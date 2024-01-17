---
title:                "Iniciando um novo projeto"
html_title:           "Arduino: Iniciando um novo projeto"
simple_title:         "Iniciando um novo projeto"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/starting-a-new-project.md"
---

{{< edit_this_page >}}

## O que & Por que?

Começar um novo projeto no Arduino é simplesmente criar e programar um novo circuito usando o microcontrolador Arduino. Os programadores o fazem para criar dispositivos personalizados e automatizar tarefas com facilidade.

## Como fazer:

```
Arduino.setup(); 
```
Esta é a função que inicia um novo projeto no Arduino. Ela define os parâmetros iniciais como o tipo de placa e a porta serial. 

```
void loop(){
//código a ser executado
}
```
Dentro da função "loop", o código principal do seu projeto é escrito. Ela é responsável por executar repetidamente o código até que o Arduino seja desligado.

```
digitalWrite(LED_BUILTIN, HIGH);
```
Este exemplo mostra como acender o LED embutido na placa do Arduino. O "digitalWrite" envia um sinal de alta tensão para o pino especificado, iluminando o LED.

## Profundidade de Mergulho:

O Arduino foi criado por Massimo Banzi e David Cuartielles em 2005, com o objetivo de tornar a eletrônica acessível para pessoas de todas as habilidades. Existem outras placas de desenvolvimento, como o Raspberry Pi, que também podem ser utilizadas para projetos semelhantes.

A comunidade Arduino é muito ativa e existem milhares de projetos e tutoriais disponíveis online. O site oficial do Arduino possui uma vasta documentação para ajudar os iniciantes. 

Em termos de implementação, o Arduino possui uma linguagem de programação própria, baseada em C++, e uma IDE (ambiente de desenvolvimento integrado) simples e fácil de usar.

## Veja também:

- [Site oficial do Arduino](https://www.arduino.cc/)
- [Documentação do Arduino](https://www.arduino.cc/reference/en/)
- [Comunidade Arduino](https://create.arduino.cc/projecthub)