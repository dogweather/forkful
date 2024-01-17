---
title:                "Escrevendo em erro padrão"
html_title:           "Arduino: Escrevendo em erro padrão"
simple_title:         "Escrevendo em erro padrão"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

O que é e por quê escrever no erro padrão?

Escrever no erro padrão é uma técnica comum usada por programadores para imprimir mensagens de erro e informações de depuração durante a execução de um programa. Isso permite que os desenvolvedores identifiquem e resolvam problemas em seu código de maneira mais eficiente. 

Como fazer:

```
Arduino.println("Mensagem de erro ou depuração");
```

Ao usar essa função, a mensagem será impressa na janela do Monitor Serial do Arduino. Isso pode ser útil para verificar o valor de variáveis durante a execução do programa ou detectar erros em determinadas linhas de código.

Exemplo de saída:

```
Erro na linha 25: valor inválido para variável x
```

Mergulho profundo:

A escrita no erro padrão tem sido uma técnica amplamente utilizada desde os primeiros dias de programação. Ela foi desenvolvida para facilitar a identificação de erros em programas e ajudar os desenvolvedores a solucioná-los de maneira rápida e eficiente.

Uma alternativa para a escrita no erro padrão é o uso de um depurador, um software especializado para identificar e corrigir erros em código. No entanto, nem todos os ambientes de programação possuem um depurador integrado, tornando a escrita no erro padrão uma opção mais acessível para os desenvolvedores.

Outra alternativa é usar uma biblioteca, como a "DebugUtils", que fornece funções adicionais para imprimir informações de depuração na janela do Monitor Serial.

Veja também:

- Documentação oficial do Arduino para a função println: https://www.arduino.cc/reference/en/language/functions/communication/serial/println/
- Tutorial da SparkFun sobre escrita no erro padrão: https://learn.sparkfun.com/tutorials/printer-defects-im-not-defective-youre-defective/
- Documentação da biblioteca "DebugUtils": https://github.com/JoaoLopesF/DebugUtils