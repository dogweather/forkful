---
title:                "Escrevendo para o erro padrão"
html_title:           "PHP: Escrevendo para o erro padrão"
simple_title:         "Escrevendo para o erro padrão"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## O que & Porquê?

Escrever em erro padrão é um recurso importante para programadores PHP, pois permite exibir mensagens de erro e depuração no terminal, em vez de exibi-las na página do website. Isso ajuda a manter uma aparência profissional ao mesmo tempo em que fornece informações valiosas para o desenvolvimento e resolução de problemas.

## Como fazer:

Utilizar a função `fwrite()` com o parâmetro `STDERR` é a maneira mais simples de escrever em erro padrão no PHP. Por exemplo:

```
<?php
fwrite(STDERR, "Erro ao acessar o banco de dados");
```

Isso irá exibir a mensagem de erro "Erro ao acessar o banco de dados" no terminal. Também é possível utilizar a função `error_log()`, que permite registrar erros em um arquivo especificado. Por exemplo:

```
<?php
error_log("Erro ao acessar o banco de dados", 3, "./logs/erros.log");
```

Isso irá registrar a mensagem de erro no arquivo "erros.log" localizado na pasta "logs".

## Profundidade:

Antes da introdução do recurso de erro padrão, as mensagens de erro e depuração eram exibidas diretamente na página do website, o que pode criar uma experiência ruim para o usuário final. Com a escrita em erro padrão, os programadores podem manter a página limpa e profissional, enquanto ainda têm acesso a informações importantes para o desenvolvimento.

Além disso, existem outras maneiras de lidar com erros no PHP, como a utilização de exceções e o controle de erro por meio de diretivas do PHP.ini.

## Veja também:

Para mais informações sobre escrita em erro padrão no PHP, consulte a documentação oficial: [https://www.php.net/manual/pt_BR/wrappers.php.php](https://www.php.net/manual/pt_BR/wrappers.php).

Também é possível conferir um exemplo prático de como utilizar a função `fwrite()` para escrever em erro padrão neste artigo: [https://www.php.net/manual/pt_BR/function.fwrite.php](https://www.php.net/manual/pt_BR/function.fwrite.php).