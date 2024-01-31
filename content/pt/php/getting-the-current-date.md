---
title:                "Obtendo a data atual"
date:                  2024-01-20T15:16:12.125192-07:00
html_title:           "Bash: Obtendo a data atual"
simple_title:         "Obtendo a data atual"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## O Quê & Porquê?
Obter a data atual em PHP é pegar o dia, mês e ano em que o código está sendo executado. Programadores fazem isso para registrar eventos, validar sessões, criar timestamps e personalizar conteúdo para o usuário.

## Como fazer:
```PHP
<?php
echo "Hoje é: " . date("d/m/Y") . "<br>";
echo "Agora são: " . date("H:i:s");
?>
```
Saída de exemplo:
```
Hoje é: 10/04/2023
Agora são: 15:42:01
```

## Mergulho Profundo
Antes de PHP 5.2.0, a função `date_default_timezone_get()` requer a diretiva `date.timezone` no arquivo php.ini para obter o fusohorário correto. Atualmente, com `date_default_timezone_set('America/Sao_Paulo')`, você pode definir o fusohorário no próprio script. Alternativamente, `DateTime` e `DateTimeZone` objetos oferecem uma abordagem orientada a objetos. É importante usar essas funções por questões de internacionalização e para evitar problemas com horário de verão e fusos horários.

## Veja Também
- [Documentação oficial da função date](https://www.php.net/manual/pt_BR/function.date.php)
- [Como configurar o fuso horário no PHP](https://www.php.net/manual/pt_BR/function.date-default-timezone-set.php)
- [Classe DateTime](https://www.php.net/manual/pt_BR/class.datetime.php)
- [Classe DateTimeZone](https://www.php.net/manual/pt_BR/class.datetimezone.php)
