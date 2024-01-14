---
title:    "Arduino: Imprimindo saída de depuração"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por que

Printar saídas de debug é uma ferramenta essencial para o desenvolvimento e depuração de programas no seu Arduino. Com essas saídas, é possível ver passo a passo o comportamento do código e identificar possíveis erros ou bugs.

## Como Fazer

Para imprimir saídas de debug no seu Arduino, primeiro devemos identificar qual o método de comunicação usado entre o Arduino e o dispositivo que exibe as saídas (como um computador ou monitor). Existem duas formas principais de comunicação: Serial e LCD. 

### Serial

Para imprimir saídas de debug através da porta serial, você pode usar a função `Serial.print()` seguida da informação que deseja exibir como parâmetro. Por exemplo:

```Arduino
int temperature = 25;
Serial.print("A temperatura atual é: ");
Serial.print(temperature);
```

Neste caso, a saída seria "A temperatura atual é: 25" na porta serial. Você também pode usar a função `Serial.println()` para adicionar uma quebra de linha depois da informação impressa.

### LCD

Se você estiver usando um display LCD para imprimir saídas de debug, a função `lcd.print()` deve ser usada da mesma forma que a função `Serial.print()`. Por exemplo:

```Arduino
int counter = 0;
lcd.print("Contagem: ");
lcd.print(counter);
```

## Deep Dive

Além dos métodos de comunicação, é importante lembrar que também é necessário inicializar a comunicação serial ou ativar o display LCD no início do código. Outro ponto importante é que você pode imprimir não apenas variáveis, mas também mensagens de depuração como avisos ou erros para ajudar no processo de depuração.

## Veja Também

- [Documentação Oficial do Arduino para Serial](https://www.arduino.cc/reference/pt/language/functions/communication/serial/)
- [Tutorial para imprimir saídas de debug com display LCD](https://www.arduino.cc/en/Tutorial/HelloWorld)
- [Vídeo tutorial sobre como usar saídas de debug no seu projeto Arduino](https://www.youtube.com/watch?v=5C7JjIpXEfo)