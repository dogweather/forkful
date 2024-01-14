---
title:                "PHP: Escrevendo no erro padrão"
programming_language: "PHP"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que escrever no fluxo de erro padrão (standard error)?

Escrever no fluxo de erro padrão é uma prática essencial para programadores PHP. Isso permite que você capture e manipule os erros lançados pelo seu código, tornando a depuração mais fácil e eficiente.

## Como fazer:

Para escrever no fluxo de erro padrão em PHP, você pode usar a função `fwrite()`. Veja um exemplo abaixo:

```PHP
// abrindo o arquivo de erro
$handle = fopen('php://stderr', 'w');

// escrevendo uma mensagem de erro
fwrite($handle, 'Algo deu errado!');

// fechando o arquivo
fclose($handle);
```

O código acima abre o fluxo de erro padrão e escreve uma mensagem nele. Lembre-se de que o segundo argumento da função `fopen()` é definido como `'w'`, o que permite que você escreva no arquivo.

## Mergulho profundo:

Existem certos casos em que escrever no fluxo de erro padrão se torna ainda mais importante. Por exemplo, se você estiver executando scripts em um servidor sem interface gráfica, o fluxo de erro padrão é a única maneira de ver os erros. Além disso, é possível redirecionar o fluxo de erro padrão para um arquivo de log, permitindo que você mantenha um registro dos erros em suas aplicações.

## Veja também:

- [Documentação oficial do PHP sobre fwrite()](https://www.php.net/manual/pt_BR/function.fwrite.php)
- [Artigo sobre como redirecionar o fluxo de erro padrão](https://www.php.net/manual/pt_BR/errorfunc.configuration.php#ini.display-errors) (em inglês)
- [Tutorial sobre depuração de código no PHP](https://www.php.net/manual/pt_BR/debugger.php) (em português)