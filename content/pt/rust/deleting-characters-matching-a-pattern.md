---
title:                "Excluindo caracteres que correspondem a um padrão"
html_title:           "Arduino: Excluindo caracteres que correspondem a um padrão"
simple_title:         "Excluindo caracteres que correspondem a um padrão"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Apagando Caracteres Correspondentes a um Padrão em Rust

## O Que & Porquê?

Apagar caracteres que correspondem a um padrão se refere a eliminar caracteres específicos de uma string. Programadores fazem isso para limpar dados, da mesma maneira, remover caracteres indesejados.

## Como Fazer:

Para excluir caracteres que coincidem com um padrão em Rust, usaríamos o método `replace()`. Este método substitui todas as ocorrências de um padrão por outro. Para excluir caracteres específicos, substituímos o padrão por uma string vazia `""`.

```Rust
fn main() {
    let minha_string = "Rust é incrível!";
    let nova_string = minha_string.replace("incrível", "");
    println!("{}", nova_string);
}
```
Output:
```
Rust é !
```

## Mergulho Profundo

**Contexto Histórico:** Em versões anteriores do Rust, a manipulação de string era mais complicada e menos intuitiva, mas as atualizações recentes simplificaram o processo.

**Alternativas:** Uma alternativa ao método `replace()` é usar expressões regulares com a biblioteca `regex`:

```Rust
use regex::Regex;

fn main() {
    let re = Regex::new("incrível").unwrap();
    let result = re.replace_all("Rust é incrível!", "");
    println!("{}", result);
}
```
Output:
```
Rust é !
```

**Detalhes de Implementação:** O método `replace()` é apenas um envoltório em torno de um loop que percorre cada caractere da string, verificando se ele corresponde ao padrão e, se corresponder, substitui-o.

## Ver Também

Para mais informações sobre manipulação de strings em Rust, confira estes links:

- Documentação do Rust: [String](https://doc.rust-lang.org/std/string/struct.String.html) 
- Blog "Learning Rust": Artigo [Strings](https://learning-rust.github.io/docs/c2.strings.html)
- Stack Overflow: [Tag Rust](https://stackoverflow.com/questions/tagged/rust)