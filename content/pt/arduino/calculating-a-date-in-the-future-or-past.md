---
title:    "Arduino: Calculando uma data no futuro ou passado"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por que calcular uma data no futuro ou passado?

O Arduino é uma plataforma de prototipagem eletrônica muito popular entre os entusiastas da tecnologia e programação. Muitos projetos envolvem o uso de datas, como por exemplo sistemas de agendamento ou alarmes. Saber como calcular uma data no futuro ou passado é essencial para criar projetos mais avançados com o Arduino.

## Como fazer:

Calculando uma data no futuro ou passado pode parecer um pouco complicado, mas com o Arduino é mais fácil do que você imagina. Para isso, primeiro é necessário definir as variáveis utilizadas, como dia, mês e ano. Em seguida, utilize a função ```day()```, ```month()``` e ```year()``` para obter os valores atuais. Veja o exemplo abaixo:

```Arduino
int diaAtual = day();
int mesAtual = month();
int anoAtual = year();
```

Em seguida, defina quantos dias você deseja avançar ou retroceder. Por exemplo, se quiser calcular a data daqui a uma semana, você deve adicionar 7 dias à variável ```diaAtual```. Agora, basta utilizar a função ```dateAdd()``` para calcular a data resultante. Veja o exemplo completo abaixo:

```Arduino
int diaAtual = day();
int mesAtual = month();
int anoAtual = year();

int diasADicionar = 7;
int dataResultante = dateAdd(diaAtual, mesAtual, anoAtual, diasADicionar);
```

O valor final da variável ```dataResultante``` será a data calculada. Você também pode utilizar essa lógica para retroceder uma data, basta utilizar um valor negativo em ```diasADicionar```.

## Mergulho profundo:

É importante lembrar que essa lógica funciona apenas com datas no formato dia/mês/ano. Se estiver utilizando um formato diferente, como mês/dia/ano, será necessário adaptar o código para utilizar as funções corretas. Além disso, é importante ter em mente que essa lógica pode não funcionar corretamente durante mudanças de ano bissexto, já que a quantidade de dias em fevereiro varia. Sempre teste seu código em diferentes cenários para garantir que ele funcione corretamente.

## Veja também:

- [Tutorial de datas e horas no Arduino](https://www.arduino.cc/en/Tutorial/Date)
- [Função ```day()``` - Documentação do Arduino](https://www.arduino.cc/reference/en/language/functions/time/day/)
- [Função ```month()``` - Documentação do Arduino](https://www.arduino.cc/reference/en/language/functions/time/month/)
- [Função ```year()``` - Documentação do Arduino](https://www.arduino.cc/reference/en/language/functions/time/year/)