---
title:    "PHP: Escrita no erro padrão"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Por que escrever para o erro padrão?

Escrever para o erro padrão pode ser um recurso útil para programadores PHP que precisam depurar e encontrar possíveis erros em seus códigos. Ao escrever para o erro padrão, você pode obter informações valiosas sobre o fluxo de execução do seu programa e identificar possíveis problemas.

## Como fazer

Para escrever para o erro padrão em PHP, você pode usar a função `error_log()` seguida do texto que deseja imprimir. Por exemplo:

```
<?php
    error_log("Ops, um erro ocorreu!");
?>
```

Ao executar este código, o texto "Ops, um erro ocorreu!" será impresso no arquivo de log do servidor, que geralmente é o arquivo `error.log`.

Você também pode definir o log em um arquivo específico, especificando o caminho absoluto do arquivo na função `error_log()`, como mostrado no exemplo abaixo:

```
<?php
    error_log("Erro de conexão com o banco de dados", 3, "/var/log/erros.log");
?>
```

O primeiro parâmetro é a mensagem de erro, o segundo é o tipo de log (3 representa o log em um arquivo) e o terceiro é o caminho absoluto do arquivo de log.

## Aprofundando-se

Além de imprimir simples mensagens de erro, você também pode usar a função `error_log()` para registrar variáveis e valores de uma forma mais estruturada. Por exemplo:

```
<?php
    $num1 = 10;
    $num2 = 5;
    error_log("O valor de $num1 é $num2");
?>
```

Este código irá imprimir no arquivo de log: "O valor de 10 é 5". Isso pode ser útil ao tentar identificar onde e como uma variável está sendo alterada durante a execução do seu código.

Além disso, você também pode usar o parâmetro `message_type` para especificar o tipo de erro que está sendo registrado. Isso pode ajudá-lo a organizar seu arquivo de log e diferenciar entre diferentes tipos de mensagens.

## Veja também

- [Documentação do PHP sobre a função error_log](https://www.php.net/manual/pt_BR/function.error-log.php)
- [Tutorial sobre escrita para o erro padrão em PHP](https://www.phptherightway.com/#errors_and_exceptions)