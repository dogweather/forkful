---
title:                "PHP: Escrevendo para o erro padrão"
simple_title:         "Escrevendo para o erro padrão"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que escrever para o erro padrão?

Escrever para o erro padrão é uma técnica essencial em programação PHP. Quando se trata de encontrar bugs e depurar código, é importante ter uma maneira confiável de identificar e solucionar problemas. Isso é exatamente o que escrever para o erro padrão permite fazer.

## Como fazer

Para escrever para o erro padrão em PHP, utilize a função `fwrite ()` com o ponteiro do arquivo "php://stderr". Por exemplo, este código irá escrever a mensagem "Erro de sintaxe!" para o erro padrão:

```PHP
<?php
fwrite (fopen ('php://stderr', 'w'), 'Erro de sintaxe!');
?>
```

A saída deve ser semelhante a:
```
Erro de sintaxe!
```

Você também pode utilizar a função `error_log ()` para escrever para o arquivo de log do erro padrão. Este código irá escrever a mensagem "Erro de conexão ao banco de dados!" no log do erro padrão:

```PHP
<?php
error_log ('Erro de conexão ao banco de dados!', 0);
?>
```

## Mergulho profundo

Escrever para o erro padrão também pode ser útil para capturar exceções em código que não está dentro de um bloco `try-catch`. Ao usar a função `set_exception_handler ()`, você pode definir uma função personalizada para manipular as exceções não capturadas. Esta função pode, então, escrever a exceção para o erro padrão e gravá-la em um arquivo de log.

Outra dica útil é aproveitar as capacidades de personalização do `error_log ()`. Você pode especificar um arquivo de log diferente ou adicionar informações extras, como a hora e a data, à mensagem de erro. Isso pode ser útil para depurar problemas em tempo real.

## Veja também

- [Documentação do PHP sobre `fwrite ()`](https://www.php.net/manual/pt_BR/function.fwrite.php)
- [Documentação do PHP sobre `error_log ()`](https://www.php.net/manual/pt_BR/function.error-log.php)
- [Artigo sobre como depurar código PHP com o erro padrão](https://www.sitepoint.com/debug-php-errors-basics/)