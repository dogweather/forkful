---
date: 2024-01-26 04:26:21.103201-07:00
description: "TOML \xE9 uma linguagem de serializa\xE7\xE3o de dados leg\xEDvel por\
  \ humanos, frequentemente utilizada para configura\xE7\xF5es. Programadores usam\
  \ TOML pela sua\u2026"
lastmod: '2024-03-13T22:44:46.391313-06:00'
model: gpt-4-0125-preview
summary: "TOML \xE9 uma linguagem de serializa\xE7\xE3o de dados leg\xEDvel por humanos,\
  \ frequentemente utilizada para configura\xE7\xF5es. Programadores usam TOML pela\
  \ sua\u2026"
title: Trabalhando com TOML
---

{{< edit_this_page >}}

## O Que e Por Que?
TOML é uma linguagem de serialização de dados legível por humanos, frequentemente utilizada para configurações. Programadores usam TOML pela sua simplicidade e clareza, traduzindo-se facilmente em um hash map em Rust.

## Como fazer:
```Rust
// 1. Incluir o crate 'toml' no seu Cargo.toml
// [dependencies]
// toml = "0.5"

// 2. Desserializar TOML em uma struct em Rust
use toml::Value;

fn main() {
    let toml_content = r#"
        [servidor]
        host = "localhost"
        port = 8080
    "#;

    let valor = toml_content.parse::<Value>().unwrap();
    let host = valor.get("servidor").unwrap().get("host").unwrap();
    let port = valor.get("servidor").unwrap().get("port").unwrap();
    
    println!("O servidor está rodando em {}:{}", host, port);
    // Saída: O servidor está rodando em "localhost":8080
}
```

## Aprofundamento
TOML, que significa Tom's Obvious, Minimal Language (Linguagem Mínima e Óbvia do Tom), foi criada por Tom Preston-Werner em 2013. O objetivo é ser mais legível que JSON ou YAML para arquivos de configuração. O design do TOML foca em uma sintaxe inequívoca, minimalismo e mapeamento fácil para tipos de dados.

Alternativas para o TOML incluem JSON, YAML e XML, mas o TOML ganha em cenários onde a legibilidade humana e a edição de arquivos por não programadores é crucial. Ao trabalhar com TOML em Rust, serde fornece uma base sólida para serialização e desserialização, usando traits para mapear TOML em structs de Rust sem esforço.

Um desafio ao trabalhar com TOML é a sua rigidez em tipos e estrutura. O programador deve definir um sistema de tipos Rust bem estruturado que reflita o esquema dos dados TOML para utilizar efetivamente o TOML em Rust.

## Veja Também
- [Documentação do TOML](https://toml.io/pt/)
- [Crate serde_toml](https://docs.rs/serde_toml/)
- [Livro da Linguagem de Programação Rust](https://doc.rust-lang.org/stable/book/)
- [Repositório TOML no GitHub](https://github.com/toml-lang/toml)
