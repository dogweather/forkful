---
title:                "Começando um novo projeto"
html_title:           "Arduino: Começando um novo projeto"
simple_title:         "Começando um novo projeto"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Por que começar um novo projeto no Arduino?

Há várias razões pelas quais alguém pode querer começar um novo projeto no Arduino. Pode ser para desenvolver um protótipo de uma ideia, aprender sobre programação e eletrônica, ou simplesmente se divertir criando algo novo. Seja qual for a razão, o Arduino é uma plataforma acessível e versátil para iniciantes e especialistas em tecnologia.

## Como fazer?

Começar um novo projeto no Arduino requer apenas alguns passos simples. Primeiro, é importante ter um Arduino Board, que é o hardware principal da plataforma. Em seguida, conecte o Arduino ao seu computador usando um cabo USB. Agora, basta seguir estes passos para começar a codificar:

```
Arduino setup() {
  pinMode(ledPin, OUTPUT); // Definir o pino do LED como saída
}

Arduino loop() {
  digitalWrite(ledPin, HIGH); // Ligar o LED
  delay(1000); // Esperar 1 segundo
  digitalWrite(ledPin, LOW); // Desligar o LED
  delay(1000); // Esperar 1 segundo
}
```

Ao fazer o upload deste código simples para o Arduino, um LED conectado ao pino especificado irá piscar a cada segundo. Isso é apenas um exemplo básico de como o Arduino pode ser programado para controlar dispositivos externos.

## Mergulho profundo

Iniciar um novo projeto no Arduino pode ser intimidante para iniciantes, mas existem muitos recursos disponíveis para ajudá-lo a dar os primeiros passos. O site oficial do Arduino tem uma seção de documentação abrangente, incluindo tutoriais e exemplos de código para vários projetos. Além disso, há uma comunidade ativa de usuários do Arduino que estão dispostos a compartilhar conhecimentos e ajudar uns aos outros.

## Veja também

- [Site oficial do Arduino](https://www.arduino.cc/)
- [Documentação do Arduino](https://www.arduino.cc/reference/en/)
- [Fórum do Arduino](https://forum.arduino.cc/)