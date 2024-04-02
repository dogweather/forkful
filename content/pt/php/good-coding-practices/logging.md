---
date: 2024-01-26 01:07:05.330672-07:00
description: "O registro de atividades (logging) \xE9 basicamente como manter um di\xE1\
  rio para o seu c\xF3digo; \xE9 o ato de gravar eventos, erros e outros pontos de\
  \ dados\u2026"
lastmod: '2024-03-13T22:44:46.673817-06:00'
model: gpt-4-1106-preview
summary: "O registro de atividades (logging) \xE9 basicamente como manter um di\xE1\
  rio para o seu c\xF3digo; \xE9 o ato de gravar eventos, erros e outros pontos de\
  \ dados\u2026"
title: Registro de Logs
weight: 17
---

## O Quê & Porquê?

O registro de atividades (logging) é basicamente como manter um diário para o seu código; é o ato de gravar eventos, erros e outros pontos de dados significativos que acontecem quando sua aplicação está em execução. Os programadores fazem isso para acompanhar o que está acontecendo sob o capô, depurar problemas e manter um registro de auditoria para análises posteriores ou para fins de conformidade.

## Como Fazer:

O PHP vem com uma função de registro de erros embutida que é fácil de usar. Basta inserir `error_log()` no seu código para enviar uma mensagem aos registros de servidor. Você também pode personalizá-la para escrever em um arquivo específico.

```php
<?php
// Registrando uma mensagem simples de informação
error_log("This is an info log entry.");

// Registrando uma mensagem de erro
error_log("This is an error log entry.", 0);

// Registrando em um arquivo especificado
file_put_contents('/caminho/para/seu/custom.log', "A custom log entry.\n", FILE_APPEND);

// Usando Monolog para registro estruturado
require 'vendor/autoload.php';
use Monolog\Logger;
use Monolog\Handler\StreamHandler;

// Crie o registrador
$logger = new Logger('name');
// Agora adicione alguns manipuladores
$logger->pushHandler(new StreamHandler('/caminho/para/seu/monolog.log', Logger::WARNING));

// Agora você pode usar seu registrador
$logger->warning('This is a warning log!');
$logger->error('This is an error log!');
?>
```

Isso fará a saída dos seus registros para o log do servidor ou para o seu arquivo especificado em formato de texto puro.

## Mergulho Profundo:

Historicamente, desenvolvedores PHP confiavam na função `error_log()` ou nos registros do Apache/Nginx para capturar problemas, mas isso pode ser caótico com a necessidade de analisar arquivos de texto puro e sem uma maneira fácil de filtrá-los ou ordená-los. Entram bibliotecas de registro como o Monolog, que inauguraram a era do registro estruturado no PHP. Essas soluções oferecem um melhor controle ao disponibilizarem múltiplos canais de registro, níveis de severidade e saídas formatadas (como JSON, que é um sonho para a análise programática).

Alternativas ao Monolog incluem Log4php, KLogger e o Log4php da Apache. Em termos de implementação, um registro robusto exige não apenas despejar dados em qualquer lugar, mas considerar coisas como a rotação de registros, estratégias de arquivamento e integração com ferramentas de monitoramento para realmente ser útil.

Você deve manter em mente a [Interface de Logger PSR-3](https://www.php-fig.org/psr/psr-3/), que delineia uma interface comum para bibliotecas de registro, garantindo interoperabilidade e uma maneira consistente de acessar mecanismos de registro.

## Veja Também:

- [Repositório GitHub do Monolog](https://github.com/Seldaek/monolog)
- [Especificação da Interface de Logger PSR-3](https://www.php-fig.org/psr/psr-3/)
- [Documentação do Error Log do PHP](https://www.php.net/manual/en/function.error-log.php)
- [KLogger: Uma Classe Simples de Logging para PHP](https://github.com/katzgrau/KLogger)
- [Log4php: Um framework de registro versátil para PHP](https://logging.apache.org/log4php/)

Comece com as funções embutidas, mas para uma abordagem mais sustentável e escalável, considere investir tempo para se familiarizar com uma biblioteca como o Monolog. Felizes registros!
