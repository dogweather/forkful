---
date: 2024-01-26 01:07:49.007916-07:00
description: "Registrar eventos em programa\xE7\xE3o \xE9 como manter um di\xE1rio\
  \ para a sua aplica\xE7\xE3o. \xC9 o registro sistem\xE1tico de eventos, mensagens\
  \ e pontos de dados que lhe\u2026"
lastmod: '2024-02-25T18:49:44.718405-07:00'
model: gpt-4-1106-preview
summary: "Registrar eventos em programa\xE7\xE3o \xE9 como manter um di\xE1rio para\
  \ a sua aplica\xE7\xE3o. \xC9 o registro sistem\xE1tico de eventos, mensagens e\
  \ pontos de dados que lhe\u2026"
title: Registro de Logs
---

{{< edit_this_page >}}

## O Que & Porquê?
Registrar eventos em programação é como manter um diário para a sua aplicação. É o registro sistemático de eventos, mensagens e pontos de dados que lhe dão uma visão sobre o que sua aplicação está fazendo e como está se comportando. Os programadores registram eventos porque é crucial para depuração, monitoramento da saúde da aplicação e obtenção de pistas sobre problemas potenciais antes que eles se transformem em problemas reais.

## Como fazer:
Ruby vem com um módulo embutido para registro de eventos, `Logger`, que é super fácil de usar. Aqui está um exemplo rápido para começar:

```ruby
require 'logger'

# Criar um Logger que forneça saída para STDOUT
logger = Logger.new(STDOUT)
logger.level = Logger::INFO

# Exemplos de mensagens de log
logger.info("Esta é uma mensagem de informação")
logger.warn("Esta é uma mensagem de aviso")
logger.error("Esta é uma mensagem de erro")
```

Executar o script acima resultará em uma saída como esta:

```
I, [2023-03-15T10:00:00.123456 #1234]  INFO -- : Esta é uma mensagem de informação
W, [2023-03-15T10:00:01.234567 #1234]  WARN -- : Esta é uma mensagem de aviso
E, [2023-03-15T10:00:02.345678 #1234] ERROR -- : Esta é uma mensagem de erro
```

Você pode configurar o formato de log e o nível para filtrar ruídos desnecessários e pode direcionar os logs para diferentes saídas, como um arquivo ou até mesmo um serviço de registro externo.

## Aprofundamento
Registrar eventos é uma tradição antiga na programação. Historicamente, os registros eram simples arquivos de texto, analisados manualmente com ferramentas como `grep`. Mas o conceito evoluiu para um ecossistema inteiro de frameworks e serviços de registro robustos como Log4j, Syslog no Linux, ou Sematext e Loggly na era da nuvem.

O `Logger` do Ruby é uma maneira sem frescuras de começar, mas se você precisa de mais capacidade e flexibilidade, você pode conferir alternativas como Lograge ou Semantic Logger. Essas bibliotecas se integram bem com aplicações Ruby, oferecendo um controle mais granular sobre a formatação de logs, incluindo logs estruturados (formato JSON), melhor desempenho e integração sem costura com outros serviços.

Cada biblioteca de registro de Ruby tem sua própria maneira de fazer as coisas, mas por baixo dos panos, todas giram em torno da ideia de uma instância de logger que recebe mensagens. O logger lida com essas mensagens com base em níveis definidos — DEBUG, INFO, WARN, ERROR, FATAL e UNKNOWN — e decide o que fazer com elas: imprimi-las, salvá-las em um arquivo, enviá-las pela rede, etc.

## Veja Também
Para um mergulho profundo no módulo de registro embutido do Ruby, consulte a documentação oficial:

Se você está interessado em registros mais avançados ou quer explorar gems de terceiros:
- [Lograge](https://github.com/roidrage/lograge)

Para práticas gerais de registro e filosofia (não específicas ao Ruby), estes artigos são leituras atemporais:
- [Livro de Engenharia de Confiabilidade do Site do Google - Capítulo 16: Lidando com Sobrecarga](https://sre.google/sre-book/handling-overload/#log-messages)
- [O 12 Factor App - Logs](https://12factor.net/logs)
