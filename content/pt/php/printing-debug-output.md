---
title:                "Imprimindo a saída de depuração"
html_title:           "PHP: Imprimindo a saída de depuração"
simple_title:         "Imprimindo a saída de depuração"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/printing-debug-output.md"
---

{{< edit_this_page >}}

## O que é e por quê?

Imprimir saída de depuração (ou "debug output" em inglês) é uma técnica utilizada pelos programadores para visualizar informações relevantes durante a execução de seus códigos. Isso pode ser útil para identificar erros, testar lógicas e entender o fluxo de dados em uma aplicação.

## Como fazer:

Usar a função `echo` é a maneira mais simples de imprimir saída de depuração no PHP. Por exemplo:

```PHP
$nome = "João";
echo "Olá, " . $nome . "!"; // Saída: Olá, João!
```

Outra opção é utilizar a função `print_r` para imprimir valores de arrays de forma mais legível. Por exemplo:

```PHP
$frutas = array("maçã", "banana", "laranja");
print_r($frutas);
/* Saída:
Array
(
    [0] => maçã
    [1] => banana
    [2] => laranja
)
*/
```

Para imprimir informações de forma mais detalhada, podemos utilizar a função `var_dump` que mostra o tipo de dado, tamanho e valor da variável. Por exemplo:

```PHP
$idade = 25;
var_dump($idade); // Saída: int(25)
```

## Profundidade na técnica:

A impressão de saída de depuração é uma técnica bastante antiga, utilizada desde os primórdios da programação para auxiliar no processo de desenvolvimento de códigos. Além das funções já mencionadas, existem outras alternativas como `print`, `debug_printbacktrace` e `xdebug`.

Para implementar a impressão de saída de depuração de forma mais eficiente e organizada, é possível criar uma função personalizada para essa finalidade. Dessa forma, podemos utilizar essa função em diferentes partes do código e modificar facilmente a exibição das informações conforme necessário.

## Veja também:

- Documentação oficial do PHP sobre impressão de saída de depuração: https://www.php.net/manual/en/function.print-r.php
- Artigo sobre como imprimir saída de depuração de forma eficiente: https://laravel-news.com/efficient-debugging-php