---
title:    "Gleam: Lendo argumentos da linha de comando"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por que ler argumentos da linha de comando?

Ler argumentos da linha de comando é uma habilidade valiosa para qualquer programador, especialmente para aqueles que trabalham com a linguagem de programação Gleam. Ao compreender como ler e utilizar argumentos da linha de comando, você poderá criar programas que sejam mais interativos e flexíveis, tornando seu código ainda mais poderoso.

## Como fazer isso:

A leitura de argumentos da linha de comando em Gleam pode ser feita facilmente usando a função `gleam.args.parse`. Confira o exemplo abaixo que mostra como ler um argumento da linha de comando contendo um nome e imprimir uma mensagem de saudação:

```Gleam
let
  args = gleam.args.parse()
  name = case args of
    `Ok = [name] -> name
    `Error = _ -> "Amigo"
in
  IO.println("Olá, " ++ name)
```

A saída para esse código seria "Olá, Amigo" no caso de nenhum argumento ser fornecido, ou "Olá, [nome]" caso seja fornecido um argumento na linha de comando.

## Mergulho profundo:

A função `gleam.args.parse` retorna um tipo de dados especial chamado de resultado (ou "result" em inglês), que é usado para lidar com erros em Gleam. O resultado pode ser "Ok", indicando que não houve erros, ou "Error", indicando que houve algum tipo de erro. Ao definir o nome como `case args of`, estamos tratando essas duas possibilidades. Se o resultado for "Ok", então significa que um argumento da linha de comando foi fornecido. Se for "Error", podemos usar um valor padrão para o nome, nesse caso "Amigo".

## Veja também:

Para mais informações sobre argumentos da linha de comando em Gleam, consulte a documentação oficial: 
- https://gleam.run/documentation/guides/interacting-with-the-command-line
- https://gleam.run/documentation/core/args
- https://gleam.run/documentation/reference/sources_stdlib#args