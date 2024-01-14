---
title:                "PHP: Imprimindo saída de depuração"
programming_language: "PHP"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por que imprimir saída de depuração em programação PHP?

A depuração é uma parte essencial do processo de programação, pois ajuda a identificar e corrigir erros em um código. Imprimir saída de depuração em PHP permite que os desenvolvedores visualizem informações específicas durante a execução do código, facilitando a identificação de possíveis problemas e aprimorando a qualidade do resultado final.

## Como fazer:

Para imprimir saída de depuração em PHP, basta usar a função `echo` ou `print_r` e incluir a variável ou valor que deseja visualizar. Por exemplo:

```PHP
// Exemplo de uso da função echo
$nome = "João";
echo "Olá " . $nome . "! Bem-vindo ao meu blog.";
```

```PHP
// Exemplo de uso da função print_r
$frutas = array("Maçã", "Banana", "Abacaxi", "Melancia");
print_r($frutas);
```

A saída desses códigos seria:

```
Olá João! Bem-vindo ao meu blog.
Array
(
    [0] => Maçã
    [1] => Banana
    [2] => Abacaxi
    [3] => Melancia
)
```

## Mergulho profundo:

Além das funções `echo` e `print_r`, existem outras opções para imprimir saída de depuração em PHP, como a função `var_dump` que exibe informações detalhadas sobre uma variável ou expressão, incluindo o tipo de dado e seu valor. Também é possível utilizar a constante `DEBUG_BACKTRACE_IGNORE_ARGS` como segundo argumento da função `debug_print_backtrace` para obter uma pilha de chamadas de funções sem mostrar seus argumentos.

Outra dica é utilizar a ferramenta Xdebug em conjunto com o depurador de códigos como o PHPStorm, que permite acompanhar cada linha do código e visualizar o valor das variáveis em tempo real durante a execução. Isso pode ser extremamente útil para encontrar e corrigir erros difíceis.

## Veja também:

- Artigo sobre depuração em PHP: https://www.php.net/manual/pt_BR/debugger.php
- Documentação oficial do Xdebug: https://xdebug.org/
- Tutorial de depuração com PHPStorm: https://www.jetbrains.com/help/phpstorm/debugging-with-phpstorm.html

## Veja também:

- Artigo sobre debugging in PHP: https://www.php.net/manual/pt_BR/debugger.php
- Official Xdebug documentation: https://xdebug.org/
- Debugging tutorial with PHPStorm: https://www.jetbrains.com/help/phpstorm/debugging-with-phpstorm.html