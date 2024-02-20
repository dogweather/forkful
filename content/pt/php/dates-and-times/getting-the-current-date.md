---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:12.932403-07:00
description: "Obter a data atual em PHP \xE9 uma tarefa fundamental que permite recuperar\
  \ e manipular a data e a hora do sistema. Isso \xE9 crucial para fun\xE7\xF5es como\u2026"
lastmod: 2024-02-19 22:05:05.726605
model: gpt-4-0125-preview
summary: "Obter a data atual em PHP \xE9 uma tarefa fundamental que permite recuperar\
  \ e manipular a data e a hora do sistema. Isso \xE9 crucial para fun\xE7\xF5es como\u2026"
title: Obtendo a data atual
---

{{< edit_this_page >}}

## O que & Por quê?
Obter a data atual em PHP é uma tarefa fundamental que permite recuperar e manipular a data e a hora do sistema. Isso é crucial para funções como registrar logs, marcar posts com data e hora, agendar eventos ou executar operações sensíveis ao tempo em suas aplicações.

## Como fazer:
### PHP Nativo
A função embutida `date()` do PHP é a maneira mais direta de obter a data atual. Você pode formatar a data de várias maneiras especificando o parâmetro de formato.

```php
echo date("Y-m-d"); // Exibe: 2023-04-01 (por exemplo)
echo date("l, F j, Y"); // Exibe: Saturday, April 1, 2023 (Sábado, 1 de abril de 2023)
```

Para obter a data e a hora com suporte a fuso horário, você pode usar a classe `DateTime` juntamente com `DateTimeZone`.

```php
$dateTime = new DateTime('now', new DateTimeZone('America/New_York'));
echo $dateTime->format('Y-m-d H:i:s'); // Exibe: 2023-04-01 12:00:00 (por exemplo)
```

### Usando Carbon (Uma Biblioteca Terceirizada Popular)
[Carbon](https://carbon.nesbot.com/) é uma extensão simples da API para `DateTime` que fornece uma maneira mais limpa e fluente de trabalhar com datas e horas.

Primeiro, certifique-se de que o Carbon esteja instalado via Composer:
```bash
composer require nesbot/carbon
```

Então, você pode usá-lo para obter a data atual:

```php
use Carbon\Carbon;

echo Carbon::now(); // Exibe: 2023-04-01 12:00:00 (por exemplo, no formato padrão)
echo Carbon::now()->toDateString(); // Exibe: 2023-04-01
echo Carbon::now()->format('l, F j, Y'); // Exibe: Saturday, April 1, 2023 (Sábado, 1 de abril de 2023)
```

O Carbon enriquece o manejo de data e hora em PHP adicionando legibilidade e uma infinidade de funcionalidades para manipulação, comparação e formatação do tempo.
