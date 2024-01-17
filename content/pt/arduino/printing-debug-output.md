---
title:                "Imprimindo saída de depuração"
html_title:           "Arduino: Imprimindo saída de depuração"
simple_title:         "Imprimindo saída de depuração"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

# O que e por que?

Quando programamos em Arduino, é comum nos depararmos com erros e problemas que podem ser difíceis de identificar somente olhando o código. É aí que entra a impressão de saída de depuração, uma técnica que nos permite ver o que está acontecendo dentro do nosso programa enquanto ele é executado. Os programadores usam isso para encontrar e corrigir erros em seus códigos.

# Como fazer:

Para imprimir saída de depuração em um código Arduino, usamos a função ```Serial.print()```. Podemos passar qualquer variável ou valor como parâmetro para esta função e ele será impresso no Monitor Serial, que pode ser aberto na aba "Ferramentas" no ambiente de desenvolvimento do Arduino. Veja um exemplo abaixo:

```Arduino
int x = 10;
Serial.print("O valor de x é: ");
Serial.println(x);
```

Isso irá imprimir a seguinte saída no Monitor Serial:

```
O valor de x é: 10
```

# Profundando mais:

A impressão de saída de depuração é uma técnica amplamente utilizada em programação, não apenas em Arduino, mas em outras linguagens também. Ela permite que os programadores vejam o que está acontecendo em seu código enquanto ele é executado, o que é especialmente útil para encontrar e corrigir erros complexos. Além disso, é possível usar outras funções relacionadas, como ```Serial.begin()``` para configurar a taxa de transmissão e ```Serial.available()``` para verificar se há dados disponíveis para leitura.

# Veja também:

Se você quiser saber mais sobre a impressão de saída de depuração e como ela pode ser útil em seus projetos Arduino, confira os seguintes links:

- [Documentação oficial do Arduino sobre a função ```Serial.print()```](https://www.arduino.cc/reference/pt/language/functions/communication/serial/print/)
- [Artigo do blog do Arduino sobre a depuração de códigos](https://blog.arduino.cc/2018/09/10/debugging-101/)
- [Vídeo tutorial do canal Nerd Ralph sobre a depuração de códigos em Arduino](https://www.youtube.com/watch?v=ZTrgDI4H2Lo)