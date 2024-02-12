---
title:                "Escrevendo para o erro padrão"
aliases:
- /pt/elixir/writing-to-standard-error.md
date:                  2024-02-03T19:32:59.824597-07:00
model:                 gpt-4-0125-preview
simple_title:         "Escrevendo para o erro padrão"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Porquê?

Escrever no erro padrão (stderr) em Elixir é um método de direcionar mensagens de erro e diagnósticos separadamente do output principal (stdout). Os programadores usam stderr para depurar e tratar erros sem encher o output principal do programa, tornando mais fácil identificar e abordar problemas.

## Como fazer:

Em Elixir, você pode usar funções do módulo `IO` como `IO.puts/2` e `IO.warn/2` para escrever mensagens no erro padrão:

```elixir
# Escrevendo uma mensagem simples no stderr
IO.puts(:stderr, "Erro: Algo deu errado!")

# Usando IO.warn, que é mais semântico para avisos/erros
IO.warn("Aviso: Você está prestes a exceder o limite!")
```

Saída de amostra no terminal para `IO.puts/2`:
```
Erro: Algo deu errado!
```

Para `IO.warn/2`, a saída seria semelhante, mas `IO.warn/2` é projetado especificamente para avisos e pode incluir formatação adicional ou comportamento em futuras versões do Elixir.

**Usando Bibliotecas de Terceiros**

Enquanto a biblioteca padrão do Elixir geralmente é suficiente para lidar com a saída de erro padrão, você pode achar bibliotecas como `Logger` úteis para aplicações mais complexas ou para configurar diferentes níveis de log e saídas.

Exemplo usando `Logger` para saída de uma mensagem de erro:

```elixir
require Logger

# Configura o Logger para saída no stderr
Logger.configure_backend(:console, device: :stderr)

# Escrevendo uma mensagem de erro
Logger.error("Erro: Falha ao conectar ao banco de dados.")
```

Esta configuração direciona a saída do `Logger` especificamente para stderr, o que é útil para separar a logação de erros das mensagens de log padrão.
