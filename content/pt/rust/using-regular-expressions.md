---
title:                "Usando expressões regulares"
html_title:           "Rust: Usando expressões regulares"
simple_title:         "Usando expressões regulares"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por que usar expressões regulares em Rust?

Expressões regulares são padrões de texto que são usados para encontrar e manipular informações em strings. Em Rust, elas são criadas usando a biblioteca padrão `regex`, que oferece suporte para encontrar padrões complexos em strings, substituir texto e até mesmo validar entradas de usuários. É uma ferramenta poderosa e útil na programação, especialmente quando se trabalha com dados de texto.

## Como usar expressões regulares em Rust

Para usar `regex` em seu código Rust, primeiro você precisa adicionar a dependência em seu `Cargo.toml`:

```
[dependências]
regex = "1.4.5"
```

Em seguida, importe a biblioteca no topo de seu arquivo:

```
use regex::Regex;
```

Agora, vamos criar uma expressão regular para encontrar um padrão específico em uma string:

```
let texto = "Rust é uma linguagem de programação incrível!";
let padrao = Regex::new("inc[r]?ível").unwrap();

if padrao.is_match(texto) {
    println!("Encontramos o padrão na string!");
}
```

Neste exemplo, usamos o `new()` para criar uma nova instância de `Regex` com o padrão `inc[r]?ível`, o qual irá encontrar qualquer palavra que comece com "inc" e termine com "ível" ou "rível". O `unwrap()` é usado para lidar com possíveis erros durante a criação do padrão.

Agora, se quisermos substituir o padrão encontrado por outra palavra, podemos usar o método `replace()`:

```
let novo_texto = padrao.replace(texto, "fantástica").to_string();
println!("{}", novo_texto); // Rust é uma linguagem de programação fantástica!
```

Se quisermos extrair os padrões encontrados em uma string e armazená-los em um vetor, podemos usar o método `find_iter()`:

```
let texto = "Rust é uma ótima linguagem de programação!";
let padrao = Regex::new("[a-z]+").unwrap();
let mut palavras: Vec<&str> = padrão.find_iter(texto).map(|mat| mat.as_str()).collect();

println!("{:?}", palavras); // ["Rust", "é", "uma", "ótima", "linguagem", "de", "programação"]
```

Existem muitos outros métodos e funcionalidades disponíveis na biblioteca `regex` para realizarmos tarefas com expressões regulares em Rust. Vale a pena explorar a documentação para descobrir mais.

## Aprofundando em expressões regulares em Rust

Enquanto as expressões regulares são uma ferramenta poderosa e útil, elas também podem ser complexas e difíceis de compreender. Em Rust, existem algumas diferenças em relação a expressões regulares em outras linguagens de programação, como ter que lidar com "ownership" e escapar caracteres especiais.

Além disso, usar expressões regulares em códigos grandes e complexos pode resultar em perda de legibilidade e dificultar a manutenção. É importante usar expressões regulares com sabedoria e considerar se elas são a melhor solução para o problema em mãos.

## Veja também

Confira mais sobre a biblioteca `regex` em Rust e como usar expressões regulares em seu código:

- [Documentação oficial da biblioteca regex](https://docs.rs/regex/1.4.5/regex/)
- [Tutorial sobre expressões regulares em Rust](https://www.regular-expressions.info/rust.html)
- [Vídeo tutorial de expressões regulares em Rust](https://www.youtube.com/watch?v=xc9v2OAEeNw)