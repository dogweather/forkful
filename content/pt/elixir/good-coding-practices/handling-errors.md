---
date: 2024-01-26 00:52:16.263525-07:00
description: "Tratar erros significa escrever c\xF3digo que pode lidar com situa\xE7\
  \xF5es inesperadas. Os programadores fazem isso para evitar falhas e para garantir\
  \ que seus\u2026"
lastmod: '2024-03-13T22:44:46.245458-06:00'
model: gpt-4-1106-preview
summary: "Tratar erros significa escrever c\xF3digo que pode lidar com situa\xE7\xF5\
  es inesperadas. Os programadores fazem isso para evitar falhas e para garantir que\
  \ seus\u2026"
title: Tratamento de erros
---

{{< edit_this_page >}}

## O Que & Por Quê?

Tratar erros significa escrever código que pode lidar com situações inesperadas. Os programadores fazem isso para evitar falhas e para garantir que seus programas se recuperem de forma elegante quando a Lei de Murphy entra em cena.

## Como fazer:

No Elixir, frequentemente usamos correspondência de padrões e a declaração `case` para lidar com diferentes resultados, incluindo erros.

```elixir
defmodule Example do
  def divide(a, b) do
    case b do
      0 -> {:error, "Não é possível dividir por zero."}
      _ -> {:ok, a / b}
    end
  end
end

# Divisão bem-sucedida
{:ok, resultado} = Example.divide(10, 2)
IO.puts("10 / 2 é #{resultado}")

# Tentativa de dividir por zero
{:error, motivo} = Example.divide(10, 0)
IO.puts("Erro: #{motivo}")
```

Saída de exemplo:
```
10 / 2 é 5.0
Erro: Não é possível dividir por zero.
```

Quando você executa esse código Elixir, você terá um resultado de divisão bem-sucedido ou uma mensagem de erro, dependendo da sua entrada. Nada de falhas aqui!

## Aprofundamento

Há tempos atrás, o tratamento de erros era frequentemente sobre a verificação de valores de retorno. No entanto, com as raízes funcionais do Elixir, temos a correspondência de padrões e tuplas nomeadas, como `{:ok, value}` ou `{:error, reason}`, que são mais elegantes.

Há outras maneiras de tratar erros no Elixir:

- **O `try` e `rescue` do Elixir**, que se assemelham ao tradicional `try-catch` das linguagens imperativas, mas são usados menos frequentemente devido à preferência do Elixir pela explicitação.
- **Supervisores e GenServers**, que fazem parte do framework OTP do Elixir e tratam mais da tolerância a falhas. Eles monitoram o processo do código, prontos para reiniciá-lo se algo der errado.

Em termos de implementação, o Elixir se baseia na robustez do Erlang. Ele trata os erros como apenas outro tipo de mensagem a ser tratada com toda a correspondência de padrões e a bondade funcional.

## Veja Também

Para leitura adicional sobre tratamento de erros no Elixir, confira:

- O guia oficial do Elixir sobre [tratamento de erros](https://elixir-lang.org/getting-started/try-catch-and-rescue.html).
- Saiba mais sobre [processos e OTP](https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html).
- O Fórum Elixir é sempre um bom lugar para fazer perguntas: [https://elixirforum.com](https://elixirforum.com).
