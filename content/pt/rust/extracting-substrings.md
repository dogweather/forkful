---
title:                "Rust: Extraindo Substrings"
simple_title:         "Extraindo Substrings"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que extrair substrings em Rust?

Extrair substrings, ou seja, segmentos de uma string maior, pode ser útil em várias situações de programação. Alguns exemplos incluem a manipulação de dados de texto, a validação de entradas do usuário e a formatação de strings para exibição. Felizmente, Rust fornece uma maneira fácil e eficiente de realizar essa tarefa.

## Como extrair substrings em Rust

Extrair substrings em Rust é feito usando o método `slice` em uma string. Este método aceita um intervalo de índices que especifica quais caracteres devem ser extraídos. Por exemplo, se quisermos extrair os caracteres de uma string entre as posições 2 e 6, usamos o seguinte código:

```Rust
let string = "Exemplo de texto";
let substring = &string[2..6];

println!("{}", substring);

// Output: emplo
```

Note que o último índice (6) não é incluído na substring, resultando em uma substring do caractere na posição 2 até o caractere na posição 5. Se quiser incluir o último caractere, basta usar uma posição de índice maior:

```Rust
let string = "Exemplo de texto";
let substring = &string[2..7];

println!("{}", substring);

// Output: emploo
```

Também é possível extrair caracteres contando a partir do final da string. Por exemplo, se quisermos os últimos quatro caracteres de uma string, podemos usar índices negativos:

```Rust
let string = "Exemplo de texto";
let substring = &string[-4..];

println!("{}", substring);

// Output: texto
```

Além disso, se quisermos extrair até o final da string, podemos usar o símbolo `..` para indicar que não temos um índice específico:

```Rust
let string = "Exemplo de texto";
let substring = &string[2..];

println!("{}", substring);

// Output: emplo de texto
```

## Mais detalhes sobre extrair substrings em Rust

Ao extrair uma substring, é importante lembrar que o resultado é uma referência à string original, não uma string nova. Isso significa que quaisquer alterações feitas na substring também afetarão a string original. Além disso, se o intervalo especificado não estiver dentro dos limites da string original, Rust lançará um erro e o programa será encerrado.

## Veja também

- [Documentação oficial sobre a função `slice` em Rust](https://doc.rust-lang.org/std/primitive.slice.html)
- [Tutorial de Rust sobre manipulação de strings](https://doc.rust-lang.org/rust-by-example/std/str.html)