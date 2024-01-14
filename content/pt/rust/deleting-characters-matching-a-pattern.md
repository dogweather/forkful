---
title:    "Rust: Excluindo caracteres correspondentes a um padrão"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que?

Algumas vezes, ao trabalhar com linguagens de programação, podemos nos deparar com a necessidade de deletar determinados caracteres em um texto ou string. Isso pode ser útil, por exemplo, para remover pontuações desnecessárias ou caracteres inválidos em uma cadeia de caracteres.

## Como Fazer

Em Rust, podemos usar o método `replace()` da struct `String` para remover caracteres correspondentes a um padrão específico. Vejamos um exemplo:

```Rust
let texto = "Olá, mundo! #Rust";
let novo_texto = texto.replace("#", "");
println!("{}", novo_texto); // Saída: Olá, mundo! Rust
```

Neste exemplo, usamos o método `replace()` para substituir o caractere "#" por uma string vazia, removendo-o do texto original. Podemos também usar expressões regulares para fazer a correspondência de padrões mais complexos. Por exemplo:

```Rust
let texto = "1, 2, 3, 4";
let novo_texto = texto.replace(Regex::new("[0-9]").unwrap(), "");
println!("{}", novo_texto); // Saída: , , ,
```

Neste caso, usamos a biblioteca `Regex` para encontrar todos os dígitos numéricos e substituí-los por uma string vazia. Isso resulta em uma string apenas com as vírgulas.

## Mergulho Profundo

Além de substituir caracteres, também podemos usar o método `replace()` para modificar o texto de outras maneiras, como por exemplo, inserindo novos caracteres. Podemos usar a função `chars()` da struct `String` para iterar sobre cada caractere e fazer as alterações necessárias. Por exemplo:

```Rust
let texto = "Olá";
let mut novo_texto = String::new();
for c in texto.chars() {
    novo_texto.push(c);
    novo_texto.push('*');
}
println!("{}", novo_texto); // Saída: O*l*á*
```

Neste exemplo, inserimos um caractere "*" entre cada letra do texto original.

## Veja também

- [Documentação oficial do método replace() em Rust](https://doc.rust-lang.org/std/string/struct.String.html#method.replace)
- [Tutorial de Expressões Regulares em Rust](https://blog.sebastianzimmeck.de/regex-in-rust-a-beginners-tutorial/)
- [Como iterar sobre caracteres em Rust](https://doc.rust-lang.org/std/string/struct.String.html#method.chars)