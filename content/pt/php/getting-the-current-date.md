---
title:                "Obtendo a data atual"
html_title:           "C: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Pegar a data atual em PHP

## O quê & Por quê?

Pegar a data atual consiste em recuperar a data e hora correntes do sistema. Programadores fazem isso para registrar quando um evento ocorre, como um login de usuário.

## Como fazer:

Em PHP, recuperar a data atual é um processo simples. Vamos examinar um código de exemplo e a saída correspondente:

```PHP
<?php
echo date("d/m/Y");
?>
```
Este código irá mostrar a data atual no formato dia/mês/ano. Se você executar esse código hoje, a saída será:

```PHP
26/10/2021
```
## Mergulho profundo:

A função `date()` do PHP existe desde as primeiras versões da linguagem, tornando-se um recurso padrão para lidar com datas e horas. Alternativamente, você pode usar a classe DateTime, mais moderna e versátil.

```PHP
<?php
$hoje = new DateTime();
echo $hoje->format('d/m/Y');
?>
```
Note-se que a função `date()` retorna a data/hora local, a não ser que seja especificado um timezone. Para trabalhar com fusos horários diferentes, a classe DateTime é uma escolha mais adequada.

## Ver também:

- Documentação oficial PHP `date()`: https://www.php.net/manual/pt_BR/function.date.php
- Documentação oficial PHP `DateTime`: https://www.php.net/manual/pt_BR/class.datetime.php
- Tutorial sobre manipulação de datas em PHP: https://www.w3schools.com/php/php_date.asp