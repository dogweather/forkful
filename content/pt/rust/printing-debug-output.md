---
title:                "Rust: Saida de depuração de impressão"
simple_title:         "Saida de depuração de impressão"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por que imprimir saída de depuração no Rust é importante

Imprimir saída de depuração é uma prática útil para ajudar a entender o comportamento do seu código e encontrar possíveis erros. No Rust, isso é especialmente importante devido às suas características de segurança e baixo nível, que podem tornar a depuração mais desafiadora.

## Como fazer

Para imprimir saída de depuração no Rust, você pode usar a macro `println!()`. Ela funciona de maneira semelhante à função `printf()` da linguagem C e pode ser usada para imprimir valores de variáveis, estruturas e até mesmo mensagens de texto simples. Veja um exemplo abaixo:

```Rust
let nome = "João";

println!("Olá, meu nome é {}", nome);
```

A saída seria "Olá, meu nome é João" no console. Você também pode imprimir mais de uma variável, usando múltiplas chaves na mensagem:

```Rust
let idade = 25;

println!("Olá, meu nome é {} e tenho {} anos", nome, idade);
```

A saída seria "Olá, meu nome é João e tenho 25 anos".

Além da macro `println!()`, você também pode usar `eprintln!()` para imprimir saída de erro e `dbg!()` para imprimir valores de maneira mais detalhada. Não se esqueça de importar a macro que deseja usar no início do seu código, como mostrado abaixo:

```Rust
use std::println; // importar macro println!() do módulo std

let x = 10;

println!("O valor de x é {}", x);
```

## Mergulho Profundo

Quando você está depurando seu código no Rust, pode ser necessário imprimir valores de tipos de dados personalizados, como uma estrutura ou enum. Para isso, você pode implementar a trait `Debug` nos seus tipos de dados customizados. Isso permite que você use a macro `{:?}` para imprimir uma representação mais detalhada dos seus valores.

Veja um exemplo de uma estrutura implementando a trait `Debug`:

```Rust
#[derive(Debug)] // derivação da trait Debug
struct Pessoa {
    nome: String,
    idade: u8,
    altura: f32,
}

let joao = Pessoa {
    nome: String::from("João"),
    idade: 25,
    altura: 1.75,
};

println!("Detalhes da pessoa: {:?}", joao); // use a macro {:?} para imprimir a estrutura
```

E a saída seria algo como "Detalhes da pessoa: Pessoa { nome: "João", idade: 25, altura: 1.75 }".

Outra dica útil é usar a opção `?` após a macro `println!()` para lidar com possíveis erros durante a impressão. Isso pode ser feito da seguinte maneira:

```Rust
use std::fs::File;

let arquivo = File::open("arquivo.txt");

println!("Conteúdo do arquivo: {:?}", arquivo?); // o uso de ? lidará com possíveis erros
```

## Veja também

- [Documentação oficial do Rust sobre saída de depuração](https://doc.rust-lang.org/book/ch05-01-defining-structs.html#defining-and-instantiating-structs)
- [Vídeo tutorial sobre saída de depuração em Rust](https://www.youtube.com/watch?v=T1lMkvF_AOQ)
- [Artigo sobre traits do Rust para imprimir valores de maneira customizada](https://dev.to/steadylearner/how-and-why-to-use-the-trait-debug-in-rust-g4m)