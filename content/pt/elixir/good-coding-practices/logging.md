---
title:                "Registro de Logs"
aliases:
- /pt/elixir/logging.md
date:                  2024-01-26T01:02:31.826093-07:00
model:                 gpt-4-1106-preview
simple_title:         "Registro de Logs"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/logging.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Registros (logging) no desenvolvimento de software é a técnica de registrar eventos que ocorrem enquanto um programa está em execução, tipicamente em um arquivo ou sistema externo. Os programadores fazem isso para obter insights sobre o comportamento do software, solucionar problemas e manter um registro do histórico operacional, que é crucial para depuração e monitoramento da saúde das aplicações.

## Como fazer:
Em Elixir, a maneira primária de registrar informações é através do módulo `Logger` integrado. Veja como você pode usá-lo:

```elixir
defmodule MyApplication do
  require Logger

  def do_something_important(param) do
    Logger.info("Iniciando processo importante com o parâmetro: #{param}")

    # Simula trabalho sendo feito
    :timer.sleep(1000)

    Logger.debug("Processo completado.")
  rescue
    error -> Logger.error("Ocorreu um erro: #{inspect(error)}")
  end
end

# Para ver seus registros, você apenas chama a função:
MyApplication.do_something_important("MeuParametro")
```

Este simples trecho mostra como registrar em diferentes níveis (`info`, `debug` e `error`). Quando você executar isso, não verá a mensagem de debug, a menos que configure o nível do Logger para `:debug`. Por padrão, o Logger do Elixir filtra mensagens de registro abaixo de `:info`.

Uma saída de exemplo no nível `:info` poderia parecer assim:
```
14:32:40.123 [info]  Iniciando processo importante com o parâmetro: MeuParametro
14:32:41.126 [error] Ocorreu um erro: %RuntimeError{message: "erro de execução"}
```

## Aprofundando:
O `Logger` do Elixir é uma ferramenta integrada que faz parte da linguagem desde seus primeiros dias. Ele é influenciado pelos sistemas de registro de outras linguagens BEAM, como Erlang. O logger oferece diferentes níveis de registro – `:debug`, `:info`, `:warn` e `:error` – e é plugável, permitindo que diferentes backends sejam conectados para o manuseio de mensagens de registro.

Uma alternativa ao Logger integrado para cenários mais complexos é o uso de bibliotecas de registro como o `Logstash` ou o `Sentry` para Elixir, que podem fornecer recursos adicionais como rastreamento e agregação de erros em um formato mais visual. Para desenvolvimento local, os desenvolvedores de Elixir muitas vezes confiam na funcionalidade do Logger integrado pela sua simplicidade e integração com a BEAM VM.

Por baixo dos panos, o módulo Logger oferece registro assíncrono e síncrono. O registro assíncrono, que é o padrão, não bloqueia a execução da sua aplicação durante o registro das mensagens. Isso garante que o registro não afete negativamente o desempenho. No entanto, o registro síncrono pode ser habilitado para casos onde você precisa garantir que as mensagens sejam registradas na ordem em que foram enviadas.

A configuração do Logger pode ser ajustada no arquivo `config/config.exs` de uma aplicação Elixir, onde você pode definir o nível de registro, formato, metadados e mais. Lembre-se sempre de ajustar seus níveis de registro e saídas para diferentes ambientes; você não iria querer registros de debug verbosos inundando seus sistemas de produção.

## Veja também:
- A documentação oficial do Logger do Elixir: https://hexdocs.pm/logger/Logger.html
- Um post no blog sobre as melhores práticas de registro no Elixir: https://blog.appsignal.com/2020/05/06/elixir-logging-tips-and-tricks.html
- Sentry para Elixir no Hex: https://hex.pm/packages/sentry
- A lição do Elixir School sobre Logger: https://elixirschool.com/en/lessons/specifics/debugging/#logging
