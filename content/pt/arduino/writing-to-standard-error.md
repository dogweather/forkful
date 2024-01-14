---
title:                "Arduino: Escrevendo para o erro padrão"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que escrever para o erro padrão em Arduino?

Quando programamos em Arduino, muitas vezes encontramos erros ou problemas que precisamos depurar. Uma maneira de lidar com isso é escrever informações para o erro padrão, que pode nos ajudar a identificar e corrigir esses problemas. Neste artigo, vamos aprender como fazer isso e entender um pouco mais sobre a escrita para o erro padrão em Arduino.

## Como fazer: 

Para escrever para o erro padrão, podemos usar a função ```Serial.println()``` do Arduino. Essa função é usada para imprimir uma mensagem ou valor em uma linha nova na janela de monitor serial. Por exemplo:

```
Arduino Serial.println("Olá, mundo!");
```

Isso imprimirá "Olá, mundo!" na janela de monitor serial quando o código for executado. Podemos também usar esta função para imprimir valores de variáveis ​​ou outros dados que desejamos monitorar enquanto o código é executado.

## Aprofundando:

A função ```Serial.println()``` é útil para imprimir informações durante a execução do código, mas também podemos usá-la para depurar problemas. Se estivermos com dificuldade em descobrir onde nosso código está falhando, podemos adicionar impressões para o erro padrão em diferentes partes do código para saber até onde nosso programa está chegando antes de encontrar um erro.

Além disso, podemos usar a função ```Serial.print()``` para imprimir informações sem adicionar uma nova linha. Isso pode ser útil para visualizar valores de variáveis ​​em tempo real enquanto o código está sendo executado.

Por fim, é importante lembrar que, se estivermos usando a função ```Serial.println()``` para depuração, devemos remover essas linhas de código quando nosso projeto estiver funcionando corretamente. Isso pode liberar espaço de memória e melhorar o desempenho do código.

## Veja também:

- Documentação oficial do Arduino sobre a função ```Serial.println()```: https://www.arduino.cc/reference/en/language/functions/communication/serial/println/

- Tutorial sobre como usar o monitor serial do Arduino: https://www.arduino.cc/en/Tutorial/Serial

- Vídeo explicando como usar a função ```Serial.println()```: https://www.youtube.com/watch?v=DTRKXgX6Fzk