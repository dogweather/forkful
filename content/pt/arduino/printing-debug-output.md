---
title:    "Arduino: Imprimindo saída de depuração"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Por que imprimir saída de depuração em Arduino é importante?

Ao criar projetos e programar em Arduino, é comum que nos deparemos com erros e bugs que podem dificultar o funcionamento do nosso código. A saída de depuração é uma ferramenta importante que nos permite visualizar informações úteis durante a execução do programa, facilitando a identificação desses problemas e ajudando na resolução deles.

## Como fazer a impressão de saída de depuração em Arduino

Para imprimir saída de depuração em Arduino, utilizamos a função `Serial.println()`. Essa função envia texto, números ou outros dados para o monitor serial, que pode ser acessado no IDE do Arduino.

Veja um exemplo de como utilizar essa função em uma situação de depuração:

```
Arduino void setup() {
  // Inicialização do monitor serial com a taxa de comunicação de 9600 bps
  Serial.begin(9600);
}
void loop() {
  // Exemplo de impressão de saída de depuração
  int variavel = 10;
  Serial.println("O valor da variável é: " + variavel);
}
```

Ao executar esse código, a mensagem "O valor da variável é: 10" será exibida no monitor serial.

## Mais detalhes sobre a impressão de saída de depuração em Arduino

Além da função `Serial.println()`, existem outras variações como `Serial.print()` e `Serial.write()`, que possuem funcionalidades semelhantes, mas diferem na forma como os dados são enviados para o monitor serial.

Também é possível utilizar caracteres especiais, como o caractere de nova linha `\n`, para formatar a saída impressa no monitor serial.

É importante lembrar de utilizar a função `Serial.begin()` no `void setup()` antes de tentar imprimir saída de depuração, caso contrário, os dados não serão enviados corretamente.

## Veja também

- [Referência da função Serial.println() em Arduino](https://www.arduino.cc/reference/pt/language/functions/communication/serial/println/)
- [Tutorial completo sobre impressão de saída de depuração em Arduino](https://blog.arduino.cc/2016/06/08/troubleshooting-serial-print-debugging-arduino-code/)