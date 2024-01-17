---
title:                "Lendo argumentos da linha de comando"
html_title:           "PHP: Lendo argumentos da linha de comando"
simple_title:         "Lendo argumentos da linha de comando"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## O que & Por quê?
Ler argumentos da linha de comando é o processo de obter valores ou parâmetros fornecidos pelo usuário ao executar um programa em um terminal. Programadores fazem isso para tornar seus programas mais dinâmicos e adaptáveis, permitindo que os usuários personalizem o comportamento do programa de acordo com suas necessidades.

## Como fazer:
```PHP
<?php
// Exemplo de leitura de argumentos da linha de comando
// $php script.php arg1 arg2

$arg1 = $argv[1];
$arg2 = $argv[2];

echo "Argumento 1: $arg1\n";
echo "Argumento 2: $arg2\n";

/* Saída:
Argumento 1: arg1
Argumento 2: arg2
*/
?>
```

## Mergulho Profundo:
Ler argumentos da linha de comando tem sido uma prática comum desde os primeiros dias da programação em linguagem de linha de comandos. Hoje, ainda é amplamente utilizado em linguagens como PHP para criar scripts mais poderosos e flexíveis. Outras abordagens para obter valores do usuário incluem a leitura de entradas do usuário durante a execução do programa ou a criação de uma interface gráfica, mas o uso de argumentos da linha de comando continua sendo uma opção popular devido à sua simplicidade e eficiência. Em PHP, é possível acessar os argumentos da linha de comando através da variável global ```$argv``` e a função ```getopt()``` também pode ser usada para analisar os argumentos de forma mais flexível.

## Veja também:
- Documentação oficial do PHP sobre leitura de argumentos da linha de comando: https://www.php.net/manual/pt_BR/reserved.variables.argv.php
- Artigo da DigitalOcean sobre como usar argumentos da linha de comando em scripts PHP: https://www.digitalocean.com/community/tutorials/how-to-use-arguments-with-a-php-script
- Vídeo do canal Codecourse no YouTube explicando o uso de argumentos da linha de comando em detalhes: https://www.youtube.com/watch?v=kLhJLRXxbGI