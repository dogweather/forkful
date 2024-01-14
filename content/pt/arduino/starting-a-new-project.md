---
title:                "Arduino: Iniciando um novo projeto"
programming_language: "Arduino"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Por que iniciar um novo projeto Arduino?

Há muitas razões para começar um novo projeto usando a plataforma Arduino. Talvez você esteja procurando por um novo hobby, ou talvez queira criar algo que possa ajudar as pessoas. Ou talvez você apenas esteja curioso para aprender mais sobre programação e eletrônica. Não importa qual seja o motivo, o Arduino é uma plataforma incrível e versátil que pode ser usada para criar projetos incríveis.

## Como começar

Para começar a programar com o Arduino, você precisará do hardware certo e do software de programação. Você precisará de um microcontrolador Arduino, uma placa de circuito, cabos USB, componentes eletrônicos e um computador. Além disso, você precisará baixar e instalar o software Arduino IDE em seu computador.

Uma vez que tudo esteja configurado, podemos começar a escrever nosso primeiro programa Arduino. Vamos começar com um exemplo simples que pisca um LED conectado ao pino 13. O código para isso seria o seguinte:

```
Arduino programa de exemplo:

int led = 13;

void setup() {
  pinMode(led, OUTPUT);
}

void loop() {
  digitalWrite(led, HIGH);
  delay(1000);
  digitalWrite(led, LOW);
  delay(1000);
}
```

Ao carregar e executar este código em seu Arduino, o LED no pino 13 piscará a cada segundo. Isso pode parecer um exemplo simples, mas é um ótimo ponto de partida para aprender como funcionam os programas Arduino e como controlar os pinos.

## Mergulho profundo

Ter um bom entendimento dos conceitos básicos do Arduino é importante antes de se aprofundar em projetos mais complexos. Além disso, é útil aprender como usar componentes eletrônicos, como sensores e motores, para criar projetos mais avançados.

Também é importante ter um bom conhecimento de programação em geral. A linguagem utilizada para programar o Arduino é baseada na linguagem C++, portanto, ter conhecimento sobre essa linguagem pode ser bastante útil.

Além disso, existem muitos recursos online, como tutoriais, fóruns e comunidades, que podem ajudá-lo a aprender e a se aprofundar no mundo do Arduino.

## Veja também
- [Site oficial do Arduino](https://www.arduino.cc/)
- [Arduino Forum](https://forum.arduino.cc/index.php)
- [Arduino Project Hub](https://create.arduino.cc/projecthub)
- [Tutorial de iniciantes do Arduino](https://www.arduino.cc/en/Tutorial/Foundations)
- [Curso gratuito de introdução ao Arduino](https://www.coursera.org/learn/arduino)