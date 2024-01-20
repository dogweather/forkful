---
title:                "Lendo argumentos de linha de comando"
html_title:           "Arduino: Lendo argumentos de linha de comando"
simple_title:         "Lendo argumentos de linha de comando"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Ler Argumentos da Linha de Comando em Arduino: Um Guia Prático 

## O Que & Por Quê?
Ler argumentos da linha de comando permite a um programa obter dados de entrada diretamente do usuário ou de outro programa na inicialização. Isto é relevante porque torna softwares mais flexíveis e interativos.

## Como fazer:
Vamos a um exemplo básico. Infelizmente, Arduino não suporta argumentos da linha de comando nativamente, mas podemos simular isso com strings.

```Arduino
String commandArguments; 

 void setup() {                 
   Serial.begin(9600);          
 } 

 void loop() {                
   while (Serial.available()) {  
     commandArguments += (char)Serial.read();
   }
   Serial.println(commandArguments);
   commandArguments = "";
 }
```
Quando enviamos "Hello, Arduino!" pelo monitor serial, o output deve ser "Hello, Arduino!".

## Mergulho Profundo:
A leitura de argumentos da linha de comando é uma habilidade de longa data na programação, nascida junto com os primeiros sistemas operacionais de comando de linha. Entretanto, Arduino não foi feito para isso. A alternativa típica é usar entrada Serial ou entradas físicas (botões, potenciômetros, sensores).

Embora pudéssemos pensar em algo mais complexo para ler argumentos semelhantes aos da linha de comando, isso geralmente seria muito trabalho para pouca recompensa, subvertendo a simplicidade que torna o Arduino tão útil. 

## Veja Também:
- [Comunicação Serial no Arduino](https://www.arduino.cc/reference/pt/language/functions/communication/serial/)
- [Usando a entrada Serial no Arduino](https://www.arduino.cc/en/Tutorial/BuiltInExamples/SerialEvent)
- [Introdução à Programação Arduino](https://www.arduino.cc/en/Guide/Introduction)