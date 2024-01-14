---
title:    "Elixir: Escrevendo no erro padrão"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

"

## Por que escrever para o erro padrão em Elixir

Escrever para o erro padrão pode ser útil para depuração e monitoramento de erros em sua aplicação Elixir. Isso permite que você visualize informações de erro em tempo real e tome medidas para corrigir problemas em seu código.

## Como fazer

A sintaxe para escrever para o erro padrão em Elixir é simples e é semelhante em diferentes ambientes de execução. Primeiro, é necessário importar o módulo `IO`, que contém as funções de saída de erro padrão.

Em seguida, basta chamar a função `IO.puts/2` e passar a mensagem que deseja escrever como primeiro argumento e o stream de erro padrão como segundo argumento. Veja o exemplo abaixo:

```Elixir
IO.puts("Ooops, algo deu errado!", :stderr)
```

Isso imprimirá a mensagem "Ooops, algo deu errado!" no fluxo de erro padrão. Você também pode usar `IO.inspect/2` para imprimir o conteúdo de uma variável diretamente para o erro padrão. Por exemplo:

```Elixir
i = 10
IO.inspect(i, label: "i", output: :stderr)
```

A saída será: `i: 10`. Isso pode ser útil para monitorar o valor de variáveis ​​em momentos-chave durante a execução do seu código.

## Análise aprofundada

A função `IO.puts/2` descrita acima é especificamente para escrever em streams de erro padrão. No entanto, você também pode usar a função `IO.write/2` para escrever em qualquer fluxo, incluindo o erro padrão. A diferença é que `IO.write/2` não adiciona automaticamente uma nova linha após a mensagem, então você pode continuar escrevendo em uma linha existente.

Por exemplo:

```Elixir
IO.write("Este texto é", :stderr)
IO.write(" tudo", :stderr)
IO.write(" na mesma linha!", :stderr)
```

A saída será: `Este texto é tudo na mesma linha!`

Lembre-se de que usar `IO.puts/2` é o método preferido para escrever para o erro padrão, pois é mais legível e mais fácil de formatar.

## Veja também

- [Documentação oficial do Elixir sobre saída de dados](https://hexdocs.pm/elixir/IO.html#puts/2)
- [Artigo da Inaka sobre debugging em Elixir](https://inaka.net/blog/2015/08/27/debugging-elixir-in-production/)
- [Vídeo tutorial sobre escrita para o erro padrão em Elixir](https://www.youtube.com/watch?v=n8mr2l-UwdU)