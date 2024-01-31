---
title:                "Lendo argumentos da linha de comando"
date:                  2024-01-20T17:56:31.746095-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lendo argumentos da linha de comando"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Comandos na linha de comando são instruções que você insere diretamente no terminal para executar um programa com opções específicas. Os programadores fazem isso para interagir com scripts de forma rápida, passando informações e parâmetros de forma programática.

## Como Fazer:
Para pegar argumentos da linha de comando em PHP, você usa a variável global `$argv`. `$argc` conta os argumentos. Vamos ver isso em ação:

```php
<?php
// Exibe o nome do script
echo "Script: " . $argv[0] . "\n";

// Conta e exibe o número de argumentos passados
echo "Número de argumentos: " . ($argc - 1) . "\n";

// Exibe todos os argumentos (menos o nome do script)
for ($i = 1; $i < $argc; $i++) {
    echo "Argumento $i: " . $argv[$i] . "\n";
}
```

Saída de exemplo após rodar `php script.php arg1 arg2 arg3`:
```
Script: script.php
Número de argumentos: 3
Argumento 1: arg1
Argumento 2: arg2
Argumento 3: arg3
```

## Aprofundamento
No PHP, trabalhar com a linha de comando não é uma novidade. Desde os primórdios do PHP 4, existe suporte para isso. Há alternativas como a biblioteca `getopt`, que ajuda a parsear opções mais complexas. Quando falamos de detalhes de implementação, o `$argv` e `$argc` são populares pela simplicidade, mas pode ser limitado para scripts mais avançados, onde `getopt` ou mesmo bibliotecas externas podem oferecer mais flexibilidade e robustez.

## Veja Também
- Documentação PHP sobre `$argv` e `$argc`: https://www.php.net/manual/pt_BR/reserved.variables.argv.php
- Tutorial sobre a função `getopt`: https://www.php.net/manual/pt_BR/function.getopt.php
- Pacote de console do Symfony para aplicações CLI robustas: https://symfony.com/doc/current/components/console.html
- Documentação PHP para execução de linha de comando: https://www.php.net/manual/pt_BR/features.commandline.php
