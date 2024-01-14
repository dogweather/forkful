---
title:    "Rust: Extraindo substrings"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que

Ao trabalhar com strings em qualquer linguagem de programação, é comum a necessidade de extrair partes específicas dessa string. Em Rust, essa tarefa pode ser realizada de forma eficiente com a função `substring()`. Neste artigo, vamos discutir por que é importante saber como extrair substrings em Rust e como fazê-lo de maneira efetiva.

## Como Fazer

Extrair substrings em Rust pode ser feito facilmente com a função `substring()`. Primeiro, criamos uma string em que desejamos extrair a substring:

```
let frase = "Este é um exemplo de frase";
```

Em seguida, podemos usar a função `substring()` para extrair uma parte específica dessa string. Por exemplo, se quisermos extrair a palavra "exemplo", podemos usar a função da seguinte maneira:

```
let substring = frase.substring(11, 17);
println!("{}", substring);
```

O código acima irá imprimir a palavra "exemplo". A função `substring()` recebe dois argumentos: o índice inicial e o índice final da substring que desejamos extrair.

## Deep Dive

Existem algumas coisas importantes a serem observadas ao extrair substrings em Rust. Primeiro, é importante lembrar que o índice inicial começa em 0, assim como qualquer outra linguagem de programação. Além disso, o índice final não é incluído na substring resultante. Por exemplo, se quisermos extrair a palavra "uma" da nossa string de exemplo, devemos usar os índices 8 e 11.

Também é importante estar ciente dos limites dos índices ao extrair substrings. Se usarmos um índice maior que o comprimento da string, o programa irá retornar um erro. Além disso, se o índice inicial for maior que o índice final, a função irá retornar uma string vazia.

## Veja também

Aqui estão alguns recursos adicionais que podem ser úteis ao trabalhar com a função `substring()` em Rust:

- Documentação oficial da função `substring()`: https://doc.rust-lang.org/std/string/struct.String.html#method.substring
- Tutorial sobre manipulação de strings em Rust: https://www.therustbook.com/strings-manipulating.html
- Exemplos práticos de uso da função `substring()`: https://www.dotnetperls.com/substring-rust

Com essas informações e recursos, você deverá estar pronto para começar a extrair substrings em suas strings em Rust com facilidade. Lembre-se de sempre verificar os índices e limites para garantir um código seguro e eficiente. Happy coding!