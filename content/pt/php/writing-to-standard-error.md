---
title:                "Escrevendo no erro padrão"
html_title:           "Arduino: Escrevendo no erro padrão"
simple_title:         "Escrevendo no erro padrão"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## O Que é & Por Que?
Escrever no erro padrão (stderr) é uma maneira de os programas de computador enviarem mensagens de erro ou de diagnóstico, separadas dos dados de saída comuns. Programadores fazem isso para facilitar a depuração e o monitoramento, permitindo que as mensagens de erro sejam direcionadas e registradas de forma independente.

## Como Fazer:
Para escrever para o erro padrão em PHP, você pode usar a função `fwrite()`, especificando `STDERR` como o primeiro argumento. Aqui está um exemplo:

```PHP
<?php
fwrite(STDERR, "Essa é uma mensagem de erro.\n");
?>
```

Saída esperada no console, se você executar o script PHP via linha de comando:
```
Essa é uma mensagem de erro.
```

## Mergulho Profundo
A prática de escrever para stderr vem do Unix, onde o fluxo de erro padrão é um canal de saída separado padrão, usado para emitir mensagens de erro ou diagnóstico. Alternativamente, o PHP oferece a função `error_log()`, mas ela é mais usada para enviar mensagens de erro para outros destinos, como arquivos ou servidores remotos de log. Tecnicamente, ao usar `fwrite(STDERR)`, você está interagindo diretamente com o buffer de I/O do PHP, o que pode ser mais eficiente para mensagens simples de erro.

## Veja Também
Para mais informações sobre como usar `STDERR` e `error_log()` no PHP, consulte:
- Documentação PHP sobre STDERR: https://www.php.net/manual/pt_BR/features.commandline.io-streams.php
- Documentação PHP sobre error_log: https://www.php.net/manual/pt_BR/function.error-log.php
