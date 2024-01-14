---
title:    "Rust: Capitalizando uma string"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Por que capitalizar uma string em Rust?

Capitalizar uma string em Rust é uma tarefa comum ao trabalhar com manipulação de texto. Isso significa transformar a primeira letra de cada palavra em maiúscula, enquanto mantém o restante em minúsculo. Esse processo é útil, por exemplo, para formatar nomes próprios ou títulos de texto.

## Como Fazer

Para capitalizar uma string em Rust, podemos utilizar o método `to_title_case()` da biblioteca padrão `str`. Vamos criar uma função simples que irá receber uma string como argumento e retornar uma versão capitalizada:

```Rust
fn capitalizar_string(texto: &str) -> String {
    texto.to_title_case()
}

fn main() {
    let minha_string = "olá, mundo!";
    let minha_string_capitalizada = capitalizar_string(minha_string);
    println!("{}", minha_string_capitalizada);
}
```

No código acima, utilizamos o método `to_title_case()` na string recebida pela função `capitalizar_string()`. Em seguida, chamamos essa função no `main()`, passando como argumento a variável `minha_string` e imprimindo o resultado.

Ao executar o código, a saída será `Olá, Mundo!`, com a primeira letra de cada palavra maiúscula.

## Profundando no assunto

Além do método `to_title_case()`, a biblioteca padrão `str` também possui outras funções relacionadas à capitalização de strings, como `to_uppercase()` para transformar todas as letras em maiúsculas ou `to_lowercase()` para transformá-las em minúsculas. Esses métodos podem ser úteis para diferentes propósitos, como validação de inputs de usuário ou formatações específicas.

Outra biblioteca útil para a manipulação de texto em Rust é `string_case`. Ela permite não apenas capitalizar strings, mas também converter para outros formatos, como snake case ou kebab case, além de oferecer mais opções de personalização.

## Veja também

- [Documentação oficial da biblioteca `str` em Rust](https://doc.rust-lang.org/std/primitive.str.html)
- [Documentação oficial da biblioteca `string_case` em Rust](https://docs.rs/string_case/1.0.2/string_case/)

Agora que você sabe como capitalizar uma string em Rust, é possível aplicar esse conhecimento em seus projetos de programação. Além disso, explore outras bibliotecas e métodos para encontrar diferentes formas de manipular texto em Rust. Happy coding!