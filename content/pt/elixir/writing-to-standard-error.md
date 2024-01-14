---
title:    "Elixir: Escrevendo para o erro padrão"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Por que escrever para o erro padrão?

Escrever para o erro padrão é uma habilidade extremamente útil para programadores em Elixir. Quando enfrentamos um erro em nosso código, muitas vezes o programa interrompe a execução e nos fornece uma mensagem de erro na tela do terminal. No entanto, nem sempre é possível encontrar o erro apenas olhando para a mensagem. Em vez disso, é necessário escrever para o erro padrão para entender o que está acontecendo em cada etapa do processo.

## Como fazer isso

A maneira mais comum de escrever para o erro padrão é usando a função `IO.puts/2` do módulo `IO`. Esta função espera dois argumentos: a mensagem que queremos imprimir e o dispositivo onde queremos imprimi-la. Para escrever em erro padrão, usamos o valor `:stderr` como segundo argumento. Por exemplo:

*Exemplo de código em Elixir:*

```Elixir
IO.puts("Mensagem de erro", :stderr)
```
*Saída de exemplo:*

```
Mensagem de erro
```

Observe que a mensagem é impressa em uma nova linha, pois é o comportamento padrão da função `IO.puts/2`.

## Profundidade

Agora que sabemos como escrever para o erro padrão, podemos nos aprofundar um pouco mais nesse assunto. Como mencionado anteriormente, usamos a função `IO.puts/2` para imprimir mensagens em erro padrão. No entanto, também podemos usar outras funções do módulo `IO` para controlar a formatação da mensagem e até mesmo colorir a saída.

Por exemplo, podemos usar a função `IO.inspect/2` para imprimir uma representação estruturada de um determinado valor. Isso pode nos ajudar a depurar nosso código e entender melhor como os dados estão sendo manipulados em cada etapa. Além disso, podemos usar a função `IO.ANSI.format/2` para adicionar cores à nossa saída em erro padrão. Isso pode ser útil para destacar informações importantes ou diferenciar vários tipos de mensagens.

Este é apenas um exemplo de como podemos nos aprofundar na escrita em erro padrão. Existem muitas outras funções e técnicas que podemos usar para melhorar a maneira como lidamos com erros em nossos programas Elixir.

## Veja também

- Documentação oficial do Elixir para a função `IO`
https://hexdocs.pm/elixir/IO.html
- Artigo sobre como lidar com erros em Elixir
https://taylor.facultos.com/learning-elixir/error-handling-in-elixir/
- Vídeo tutorial sobre como escrever para o erro padrão em Elixir
https://www.youtube.com/watch?v=QDZtvk3Sj3U