---
title:                "Escrevendo no erro padrão"
html_title:           "Gleam: Escrevendo no erro padrão"
simple_title:         "Escrevendo no erro padrão"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que escrever para o erro padrão?

Escrever para o erro padrão é uma prática importante em programação que permite que os desenvolvedores capturem e relatem erros em seus códigos. Isso ajuda a identificar e corrigir problemas em tempo hábil, garantindo que o software seja executado sem falhas.

## Como fazer?

Para escrever para o erro padrão em Gleam, utilizamos a função `println!` seguida do que desejamos imprimir. Por exemplo:

```
Gleam println!("Este é um exemplo de texto que será escrito para o erro padrão.")
```

Esse código irá imprimir a mensagem "Este é um exemplo de texto que será escrito para o erro padrão." na tela do console. É importante ressaltar que a mensagem será escrita na saída padrão de erro, que é diferente da saída padrão de impressão `print!`.

## Mergulho profundo

Ao escrever para o erro padrão, é importante levar em consideração o que você está imprimindo. Lembre-se de que essa mensagem será vista pelos usuários e é uma forma de comunicação importante entre o desenvolvedor e o usuário final.

Além disso, é importante que a mensagem de erro seja específica e informativa, para que o usuário entenda o que aconteceu e como pode solucionar o problema. Evite mensagens genéricas e pouco úteis, como "Erro inesperado".

## Veja também

- Documentação oficial do Gleam sobre a função `println!`: https://gleam.run/documentation/standard_library.html#println
- Guia sobre como escrever boas mensagens de erro: https://blog.codinghorror.com/curlys-law-do-one-thing/
- Exemplos de mensagens de erro bem projetadas: https://github.com/open-source-ideas/open-source-ideas/issues/381