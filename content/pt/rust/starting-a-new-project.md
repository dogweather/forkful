---
title:                "Rust: Começando um novo projeto"
programming_language: "Rust"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/starting-a-new-project.md"
---

{{< edit_this_page >}}

##Por que iniciar um novo projeto em Rust?

Rust é uma linguagem de programação moderna, segura e eficiente que tem ganhado cada vez mais popularidade. Com suas características únicas, como sistema de tipos estático e gerenciamento de memória sem coleta de lixo, Rust é ideal para desenvolvimento de sistemas e aplicativos de alto desempenho. Se você está procurando uma linguagem de programação versátil e poderosa, iniciar um novo projeto em Rust é uma ótima opção.

## Como fazer:

Para começar um novo projeto em Rust, você precisará ter o compilador Rust instalado em seu sistema. Você pode fazer o download em [rust-lang.org](https://rust-lang.org). Uma vez instalado, execute o seguinte comando em seu terminal para criar um novo projeto:

```
Rust novo meu_projeto
```

Isso criará um novo diretório com a estrutura de um projeto Rust básico. Dentro do diretório, você encontrará um arquivo `Cargo.toml`, que contém as informações do seu projeto, e um diretório `src`, onde você escreverá o código fonte. Aqui está um exemplo simples de código Rust que imprime "Olá Mundo!" na tela:

```Rust
fn main() {
    println!("Olá Mundo!");
}
```

Para executar este código, use o comando `cargo run` no diretório do seu projeto. Você também pode compilar e executar o código separadamente usando os comandos `cargo build` e `cargo run` respectivamente.

## Mergulho Profundo:

Antes de iniciar um novo projeto em Rust, é importante entender a estrutura e a sintaxe da linguagem. Rust é uma linguagem de tipos estáticos, o que significa que todas as variáveis devem ser declaradas com um tipo específico e não podem ser alteradas posteriormente. Além disso, Rust possui um sistema de gerenciamento de memória exclusivo, que garante que os programas sejam seguros e livres de erros de memória.

Outra característica importante de Rust é o conceito de propriedade (ownership). Basicamente, isso significa que cada valor criado em Rust tem um único dono e será automaticamente liberado da memória quando esse dono sair de escopo. Isso ajuda a evitar problemas comuns de gerenciamento de memória, como vazamentos e corrupção.

Finalmente, Rust também possui um sistema de gerenciamento de pacotes chamado Cargo, que facilita o gerenciamento de dependências e a construção de projetos. Com o Cargo, você pode adicionar facilmente bibliotecas externas ao seu projeto e usá-las em seu código.

## Veja também:

- [Documentação oficial do Rust](https://doc.rust-lang.org)
- [Rust Cookbook](https://rust-lang-nursery.github.io/rust-cookbook/)
- [Rust by Example](https://doc.rust-lang.org/stable/rust-by-example/)
- [Crates.io - repositório de pacotes do Rust](https://crates.io/)