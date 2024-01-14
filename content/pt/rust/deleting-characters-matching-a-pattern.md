---
title:                "Rust: Excluindo caracteres que correspondem a um padrão"
simple_title:         "Excluindo caracteres que correspondem a um padrão"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que

O Rust é uma linguagem de programação moderna e eficiente que vem ganhando cada vez mais popularidade entre os desenvolvedores. Uma das suas principais vantagens é o seu sistema de tipos fortemente estático, que evita diversos erros de programação e garante uma maior segurança no desenvolvimento de software. Além disso, o Rust também permite a utilização de linguagens de script, o que facilita muito a criação de programas com tarefas repetitivas, como a exclusão de caracteres que correspondem a um determinado padrão.

## Como fazer

Se você deseja excluir caracteres que seguem um padrão específico em alguma string em Rust, existem algumas opções que você pode utilizar. Uma delas é a função `replace` da biblioteca padrão do Rust, que pode ser usada da seguinte forma:

```Rust
let string = String::from("Olá, mundo!");
let novo_string = string.replace("l", "");

println!("{}", novo_string);
```

Neste exemplo, estamos substituindo todos os caracteres "l" da string por uma string vazia, ou seja, excluindo-os. O resultado será "Oá, mundo!", pois os caracteres "l" foram removidos.

Outra opção é utilizar a biblioteca de expressões regulares do Rust, `regex`, que oferece um conjunto de ferramentas poderosas para lidar com padrões em strings. Veja um exemplo de como excluir todos os números de uma string utilizando expressões regulares:

```Rust
use regex::Regex;

let string = String::from("123Olá456, mundo!");
let re = Regex::new(r"[0-9]").unwrap();
let novo_string = re.replace_all(&string, "");

println!("{}", novo_string);
```

O resultado será "Olá, mundo!", pois todos os números foram excluídos da string original. Vale ressaltar que a utilização de expressões regulares pode ser mais complexa do que a função `replace`, mas oferece uma grande flexibilidade para lidar com diferentes padrões.

## Viagem profunda

Para entender melhor como é feita a exclusão de caracteres em Rust, é importante saber como a linguagem lida com strings. Diferentemente de outras linguagens, onde as strings são mutáveis e podem ser modificadas diretamente, em Rust elas são imutáveis e podem se tornar bem mais complexas de se trabalhar. Por isso, quando desejamos excluir caracteres de uma string em Rust, na verdade estamos criando uma nova string a partir da original, sem os caracteres que desejamos eliminar.

Além disso, é importante entender como as strings são representadas em memória em Rust. Elas são armazenadas como um vetor de bytes, e cada caractere da string ocupa uma certa quantidade de bytes, dependendo do seu tipo e da codificação utilizada. Isso significa que, ao excluir um caractere, é preciso recalcular a posição de todos os caracteres seguintes na string. Por isso, a exclusão de caracteres pode ser uma operação custosa em termos de desempenho em strings muito extensas.

## Veja também

- Documentação da função `replace`: https://doc.rust-lang.org/std/string/struct.String.html#method.replace
- Documentação da biblioteca `regex`: https://docs.rs/regex/1.4.2/regex/
- Tutorial sobre expressões regulares em Rust: https://docs.rs/regex/1.4.1/regex/#syntax