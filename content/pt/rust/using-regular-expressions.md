---
title:    "Rust: Utilizando expressões regulares"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Por que usar expressões regulares em Rust?

Expressões regulares são uma ferramenta poderosa para manipulação de texto e padrões em programação, e o Rust oferece suporte nativo para elas. Ao usá-las, você pode economizar tempo e esforço na busca e substituição de padrões em strings.

## Como usar expressões regulares em Rust

Para usar expressões regulares em Rust, primeiro importe o módulo `regex`:

```
use regex::Regex;
```

Em seguida, você pode criar um objeto `Regex` com o padrão que deseja buscar em uma string:

```
let re = Regex::new(r"Olá ..sto").unwrap();
```

Você pode então usar métodos como `is_match()` ou `find()` para verificar se a string contém o padrão especificado e obter a posição da primeira correspondência, respectivamente:

```
let text = "Olá mundo!";
let is_match = re.is_match(text);
let first_match = re.find(text);
```

Aqui está um exemplo mais completo que usa expressões regulares para substituir um padrão em uma string e imprimir o resultado:

```
use regex::Regex;

fn main() {
    let text = "Meu email é usuário@dominio.com";
    
    let re = Regex::new(r"(\w+)@(\w+)\.com").unwrap();
    
    let result = re.replace_all(text, r"Email: $1 at $2 dot com");
    
    println!("{}", result);
}
```

Saída:
```
Meu email é Email: usuário at dominio dot com
```

## Profundando nas expressões regulares

As expressões regulares em Rust seguem a sintaxe PCRE (Perl Compatible Regular Expressions), suportando metacaracteres como `.` para representar qualquer caractere, `*` para indicar zero ou mais repetições e `+` para indicar uma ou mais repetições. Você também pode usar grupos de captura e referências a grupos em substituições.

Além disso, o Rust oferece suporte a alguns recursos adicionais, como suporte a UTF-8 e a possibilidade de especificar o tamanho máximo do texto que será pesquisado.

Para mais informações e exemplos, consulte a documentação oficial de expressões regulares em Rust: https://doc.rust-lang.org/std/regex/

## Veja também

- [Documentação oficial sobre expressões regulares em Rust](https://doc.rust-lang.org/std/regex/)
- [Tutorial de expressões regulares em Rust](https://medium.com/@mattiaerre/regular-expressions-in-rust-9b013f0b0668)
- [Vídeo explicando o uso de expressões regulares em Rust](https://www.youtube.com/watch?v=4tP3UCaJ_fc)