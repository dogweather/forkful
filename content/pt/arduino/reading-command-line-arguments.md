---
title:    "Arduino: Lendo argumentos da linha de comando"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por que ler argumentos da linha de comando?

Se você está interessado em programação e no sistema Arduino, é importante entender como a leitura de argumentos da linha de comando pode ser útil. Ao saber como lidar com essa função, você pode criar aplicações mais dinâmicas e interativas, tornando seus projetos ainda mais interessantes.

## Como fazer

Ler argumentos da linha de comando pode parecer intimidante no início, mas com a ajuda do Arduino, o processo se torna bem mais simples. Basta seguir os seguintes passos:

1. Verifique se a biblioteca "Serial" está incluída no seu código:
```Arduino 
#include <Serial.h>
```
2. Em seguida, defina uma variável para armazenar os argumentos da linha de comando:
```Arduino
String argumento;
```
3. Dentro da função ```setup()```, inicialize a comunicação serial:
```Arduino
Serial.begin(9600);
```
4. Na função ```loop()```, adicione o seguinte código para ler os argumentos da linha de comando:
```Arduino
while (Serial.available()) {
    argumento = Serial.readStringUntil('\n'); // lê até encontrar uma quebra de linha
    // faça algo com o argumento lido
}
```

Agora, sempre que enviar um argumento através da porta serial, ele será armazenado na variável ```argumento```. Você pode então usá-la para criar diferentes funcionalidades e interagir com seu projeto de forma criativa.

## Mergulho Profundo

Para uma compreensão mais profunda sobre como a leitura de argumentos da linha de comando funciona, é importante entender que, quando enviamos dados pela porta serial, eles são armazenados em um buffer. A função ```Serial.readStringUntil()``` lê esse buffer até encontrar o caractere especificado (neste caso, a quebra de linha) e retorna o conteúdo anterior a ele.

Outra função útil é a ```Serial.parseInt()```, que lê um número inteiro do buffer serial e o retorna como um valor numérico. Combinando essas funções com comandos condicionais e loops, as possibilidades de uso de argumentos da linha de comando são infinitas.

## Veja também

- [Tutorial: Usando a porta serial do Arduino](https://www.arduino.cc/en/Serial/Begin)
- [Documentação da biblioteca Serial](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [Exemplos de uso de argumentos da linha de comando no Arduino](https://arduinobasics.blogspot.com/2011/09/command-line-arguments.html)