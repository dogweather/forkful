---
title:                "Elixir: Escrevendo no erro padrão"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Por que escrever para o erro padrão em Elixir

Escrever para o erro padrão em Elixir é uma maneira poderosa de depurar e monitorar o desempenho do seu código. Isso permite que você identifique e resolva rapidamente quaisquer erros ou problemas no seu programa.

## Como fazer

Em Elixir, podemos escrever para o erro padrão usando a função `IO.puts/2` e especificando a saída como `:stderr`. Aqui está um exemplo de como podemos usar isso no nosso código:

```Elixir
IO.puts("Esta é a saída padrão", :stderr)
```

Isso irá imprimir a mensagem "Esta é a saída padrão" no console de erro. Você também pode usar interpolação de string para exibir informações dinâmicas:

```Elixir
username = "João"
IO.puts("Bem-vindo, #{username}!", :stderr)
```

Isso será impresso como "Bem-vindo, João!" no console de erro. Você também pode criar funções personalizadas para escrever para o erro padrão, permitindo que você personalize a saída conforme necessário.

## Profundidade do mergulho

Escrever para o erro padrão em Elixir é particularmente útil ao depurar código em produção, onde você não tem acesso ao console do servidor. Além disso, você também pode usar essa técnica para monitorar o desempenho do seu código em tempo real.

Uma das vantagens de escrever para o erro padrão em Elixir é que ele é rápido e eficiente em termos de recursos. Isso significa que você pode usar isso sem se preocupar com o impacto no desempenho do seu aplicativo.

# Veja também

- [Documentação oficial do Elixir sobre saída padrão](https://hexdocs.pm/elixir/IO.html#puts/2)
- [Tutoriais de Elixir do Programação funcional em 15 minutos](https://prog21.dadgum.com/30.html)
- [Exibindo mensagens de erros coloridas em Elixir](https://elixirschool.com/lessons/advanced/error-handling/)