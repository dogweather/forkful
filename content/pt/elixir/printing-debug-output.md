---
title:    "Elixir: Imprimindo saída de depuração"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por que imprimir saída de debug em Elixir?

A impressão de saída de debug é uma técnica crucial no processo de desenvolvimento de software. É uma forma de visualizar o fluxo de execução do seu código e encontrar possíveis erros e defeitos. Ao imprimir dados durante a execução do programa, você pode entender melhor como o seu código está se comportando e identificar problemas mais rapidamente.

## Como fazer isso em Elixir?

Em Elixir, imprimir a saída de debug é simples e direto. Basta usar a função `IO.inspect/2` para imprimir qualquer valor em seu código. Aqui está um exemplo básico:

```
Elixir IO.inspect/2 ``` 
iex> IO.inspect("Elixir é incrível!")
"Elixir é incrível!"
```

Você também pode imprimir múltiplos valores ao mesmo tempo, passando-os como argumentos separados para a função. Por exemplo:

```
Elixir IO.inspect/2 ``` 
iex> IO.inspect("Elixir", "é", "incrível!")
"Elixir é incrível!"
```

Além disso, você pode usar a opção `:label` para incluir um rótulo no seu output, tornando mais fácil a identificação dos dados impressos. Aqui está um exemplo:

```
Elixir IO.inspect/2 ``` 
iex> IO.inspect("Elixir", label: "Linguagem")
"Linguagem: Elixir"
```

## Uma análise mais profunda

Existem outras funções úteis para imprimir saída de debug em Elixir, como `IO.puts/2` e `IO.inspect/4`, que permitem personalizar ainda mais o seu output. Além disso, é possível definir diferentes níveis de depuração com a função `Logger.debug/1`, permitindo que você escolha quais partes do código serão impressas durante a execução.

Além disso, vale ressaltar que a impressão de saída de debug não deve ser usada como um substituto para testes adequados e depuração completa. Ela é apenas uma ferramenta adicional para ajudá-lo a entender melhor o seu código e encontrar possíveis problemas.

## Veja também

- [Documentação oficial do Elixir](https://hexdocs.pm/elixir)
- [Guia de Depuração em Elixir](https://medium.com/@tauli/elixir-debugging-basics-df8f319c0b13)
- [Vídeo tutorial sobre impressão de saída de debug em Elixir](https://www.youtube.com/watch?v=eChYMdz-rHY)