---
title:                "Imprimindo saída de debug"
html_title:           "C#: Imprimindo saída de debug"
simple_title:         "Imprimindo saída de debug"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

# Tutorial de programação Arduino: Imprimindo a Saída de Depuração  

## O que é e por quê?
A impressão da saída de depuração é um método que nos permite ver os dados processados pelo nosso código Arduino. Fazemos isso para entender se o programa está funcionando como esperado, ou para identificar onde e porque ele está falhando.

## Como fazer:
Para imprimir a saída de depuração, usamos a função `Serial.println()`. Aqui está um exemplo simples em que imprimimos o valor de um número a cada segundo:

```Arduino
void setup() {
  Serial.begin(9600);   
}

void loop() {
  for(int i=0; i<10; i++) {
    Serial.println(i);
    delay(1000);
  }
}
```
Neste exemplo, a cada segundo vamos ver um novo número na janela do Serial Monitor do Arduino IDE. Começa no 0 e vai até 9.

## Mergulhando fundo
A ideia de imprimir a saída para depuração vem do início da programação de computadores. Muitos debuggers modernos ainda se baseiam nessa prática.

Existem várias maneiras de imprimir a saída para depuração. No Arduino, usamos principalmente `Serial.print()` e `Serial.println()`. O segundo adiciona automaticamente uma nova linha ao final. 

Também é possível transmitir informações mais complexas, como variáveis de String, ou até mesmo estruturas de dados inteiras, embora isso possa exigir funções de impressão personalizadas.

## Veja também
Se você quer se aprofundar mais neste tópico, existem vários recursos úteis disponíveis:
- Visite a página de referência oficial para a biblioteca Serial do Arduino em: [https://www.arduino.cc/reference/en/language/functions/communication/serial/](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- Confira este tutorial em vídeo sobre a depuração do Arduino: [https://www.youtube.com/watch?v=fCxzA9_kg6s](https://www.youtube.com/watch?v=fCxzA9_kg6s)
- Para uma discussão mais detalhada sobre técnicas de depuração no Arduino, veja este post no fórum Arduino: [https://forum.arduino.cc/index.php?topic=396450](https://forum.arduino.cc/index.php?topic=396450)