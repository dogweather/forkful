---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:08.600820-07:00
description: "Escrever no erro padr\xE3o (stderr) em PHP diz respeito a direcionar\
  \ mensagens de erro ou diagn\xF3sticos separadamente da sa\xEDda padr\xE3o (stdout),\
  \ permitindo que\u2026"
lastmod: '2024-03-13T22:44:46.683477-06:00'
model: gpt-4-0125-preview
summary: "Escrever no erro padr\xE3o (stderr) em PHP diz respeito a direcionar mensagens\
  \ de erro ou diagn\xF3sticos separadamente da sa\xEDda padr\xE3o (stdout), permitindo\
  \ que desenvolvedores gerenciem melhor seus fluxos de sa\xEDda para fins de depura\xE7\
  \xE3o e registro."
title: "Escrevendo para o erro padr\xE3o"
weight: 25
---

## O Que & Porquê?

Escrever no erro padrão (stderr) em PHP diz respeito a direcionar mensagens de erro ou diagnósticos separadamente da saída padrão (stdout), permitindo que desenvolvedores gerenciem melhor seus fluxos de saída para fins de depuração e registro. Programadores utilizam essa técnica para garantir que mensagens de erro não interfiram na saída do programa, facilitando o monitoramento e a solução de problemas em aplicações.

## Como fazer:

Em PHP, escrever no stderr pode ser alcançado usando a função `fwrite()` juntamente com a constante predefinida `STDERR`, que representa o fluxo de saída de erros.

```php
<?php
// Escrevendo uma mensagem simples no stderr.
fwrite(STDERR, "Esta é uma mensagem de erro.\n");
```

Saída de amostra quando o script é executado a partir da linha de comando:
```
Esta é uma mensagem de erro.
```

Para demonstrar um uso mais prático, considere um cenário em que você está analisando a entrada do usuário e encontra dados inesperados:
```php
<?php
$input = 'dados inesperados';

// Simulando um erro no processamento da entrada do usuário.
if ($input === 'dados inesperados') {
    fwrite(STDERR, "Erro: Entrada inesperada recebida.\n");
    exit(1); // Saindo com um valor não nulo para indicar um erro.
}
```

Embora as capacidades internas do PHP para lidar com stderr geralmente sejam suficientes, ao lidar com aplicações mais complexas ou desejar integrar o registro de stderr com sistemas externos, bibliotecas de terceiros como o Monolog podem ser um grande aliado. Monolog é uma biblioteca de registro que pode lidar com stderr entre muitos outros alvos (arquivos, soquetes, etc.).

Usando Monolog para escrever no stderr:

Primeiro, certifique-se de ter o Monolog instalado via Composer:
```
composer require monolog/monolog
```

Em seguida, você pode configurar o Monolog para usar o `StreamHandler` direcionado para `php://stderr`:

```php
<?php
require 'vendor/autoload.php';

use Monolog\Logger;
use Monolog\Handler\StreamHandler;

// Criar um canal de registro
$log = new Logger('name');
$log->pushHandler(new StreamHandler('php://stderr', Logger::WARNING));

// Adicionar uma mensagem de registro no stderr
$log->warning('Esta é uma mensagem de aviso.');
```

O código acima utiliza o Monolog para enviar uma mensagem de aviso ao stderr, o que é particularmente útil para aplicações que requerem configurações de registro detalhadas ou monitoramento de registro externo.
