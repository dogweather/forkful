---
date: 2024-01-20 17:56:31.746095-07:00
description: "Como Fazer: Para pegar argumentos da linha de comando em PHP, voc\xEA\
  \ usa a vari\xE1vel global `$argv`. `$argc` conta os argumentos. Vamos ver isso\
  \ em a\xE7\xE3o."
lastmod: '2024-03-13T22:44:46.682482-06:00'
model: gpt-4-1106-preview
summary: "Para pegar argumentos da linha de comando em PHP, voc\xEA usa a vari\xE1\
  vel global `$argv`."
title: Lendo argumentos da linha de comando
weight: 23
---

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
