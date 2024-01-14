---
title:    "Rust: Iniciando um novo projeto"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/rust/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Por que começar um novo projeto em Rust?
Muitos programadores estão optando por utilizar a linguagem Rust em seus projetos por sua grande performance, segurança e facilidade de uso. Além disso, a comunidade ao redor de Rust é altamente ativa e sempre disposta a ajudar, tornando-o uma excelente escolha para começar um novo projeto.

## Como iniciar um projeto em Rust
Para iniciar um projeto em Rust, é necessário seguir alguns passos básicos:

1. Instale o Rust em sua máquina através do [site oficial](https://www.rust-lang.org/pt-BR/tools/install).
2. Crie um novo diretório para o seu projeto e navegue até ele através do terminal.
3. Dentro do diretório, inicialize o projeto utilizando o comando `cargo init`.
4. O Cargo é o gerenciador de pacotes e build system de Rust, ele irá criar automaticamente o arquivo `Cargo.toml` que contém as informações do projeto e o arquivo `main.rs` que é onde será escrito o código inicial do projeto.

Agora, você está pronto para começar a programar em Rust!

```Rust
fn main() {
    println!("Olá Mundo!");
}
```

É possível compilar o código utilizando o comando `cargo build` e depois executá-lo com o comando `./target/debug/nome_do_projeto`. Alternativamente, você pode utilizar o comando `cargo run` para compilar e executar o código de uma só vez.

## Mais sobre iniciar um projeto em Rust
É importante conhecer algumas das melhores práticas ao iniciar um novo projeto em Rust. Algumas dicas incluem:

- Utilizar o gerenciador de dependências do Cargo para adicionar bibliotecas externas ao projeto.
- Utilizar o sistema de tipos de Rust para garantir a segurança do código.
- Utilizar a ferramenta de formatação automática `rustfmt` para manter um código limpo e legível.

Com essas dicas em mente, você pode começar a explorar as diversas possibilidades que Rust oferece para o desenvolvimento de aplicações rápidas, seguras e confiáveis.

## Veja também
- [Site oficial de Rust](https://www.rust-lang.org/pt-BR/)
- [Documentação oficial de Rust](https://doc.rust-lang.org/stable/)
- [Repositório oficial do Cargo](https://github.com/rust-lang/cargo)