---
title:                "Rust: Concatenando strings"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por que
Você provavelmente já se deparou com a necessidade de juntar ou misturar diferentes strings em seu código. Talvez você precise criar uma mensagem personalizada para o usuário ou construir uma URL dinamicamente. Em qualquer caso, a concatenação de strings é uma habilidade fundamental em programação e é especialmente importante na linguagem Rust.

## Como fazer
Em Rust, a concatenação de strings é realizada usando o operador `+` ou o método `format!()`. Vejamos alguns exemplos de como usar essas opções:

Cenário 1: Juntando duas strings literais
```Rust
let saudacao = "Olá";
let nome = "Maria";
let mensagem = saudacao + nome;

println!("{}", mensagem);
```

Saída:
```
OláMaria
```

Cenário 2: Juntando uma string literal com uma variável
```Rust
let saudacao = "Olá";
let nome = "Maria";
let mensagem = saudacao + &nome;

println!("{}", mensagem);
```

Saída:
```
OláMaria
```

Cenário 3: Usando o método `format!()`
```Rust
let saudacao = "Olá";
let nome = "Maria";
let mensagem = format!("{} {}", saudacao, nome);

println!("{}", mensagem);
```

Saída:
```
Olá Maria
```

Observe que no terceiro cenário usamos o `format!()` para criar uma nova string contendo a concatenação das duas strings. Isso nos permite formatar a string de maneira mais flexível e é particularmente útil quando precisamos combinar mais de duas strings.

## Profundidade
Por baixo dos panos, a concatenação de duas strings em Rust envolve a criação de uma nova string que contém o conteúdo dos dois operandos. Isso é feito através da alocação de memória para a nova string e a cópia dos bytes das duas strings originais para a nova string.

Mas e quanto ao desempenho? Vale a pena usar o operador `+` ou o método `format!()`? Na verdade, em Rust, a concatenação de strings é otimizada em tempo de compilação para usar o método `format!()` em vez do operador `+`.

Além disso, Rust também fornece uma maneira de compilar strings em tempo de execução sem alocar nova memória ou copiar bytes. Isso é feito usando o tipo `String` e seu método `push_str()` para adicionar uma string a uma já existente.

Em resumo, a concatenação de strings em Rust é eficiente e pode ser facilmente realizada usando o operador `+` ou o método `format!()`. No entanto, é importante ter em mente as opções mais eficientes, como o uso do tipo `String` e o método `push_str()`.

## Veja também
- [Documentação oficial do Rust sobre strings](https://doc.rust-lang.org/std/string/index.html)
- [Explicação sobre tipos e alocação de memória em Rust](https://blog.jimb.xyz/spotlight-on-rust-strings/)
- [Análise de desempenho da concatenação de strings em diferentes linguagens de programação](https://hackernoon.com/the-curious-case-of-concatenating-strings-javascript-python-go-ruby-and-rust-b05401b0ba82)