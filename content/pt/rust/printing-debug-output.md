---
title:                "Rust: Imprimindo saídas de depuração"
programming_language: "Rust"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

# Por que imprimir saída de depuração em Rust?

A impressão de saída de depuração é uma técnica comum e útil para verificar o funcionamento interno de um programa. Ao imprimir valores específicos em momentos importantes do código, é possível identificar e corrigir erros com mais facilidade. Em Rust, a impressão de saída de depuração também é conhecida como "impressão de depuração de controle". Agora, vamos aprender como fazer isso!

# Como fazer:

```Rust
fn main() {
    let nome = "Rust";
    let versao = 1.55;
    println!("Iniciando programa com {} versão {}...", nome, versao);
}
```

Neste exemplo, usamos a função `println!` para imprimir uma mensagem com alguns valores de variáveis. O `!` indica que estamos usando uma macro em vez de uma função comum. Isso é importante para a impressão de saída de depuração, pois permite que usemos formatos de impressão mais avançados, como veremos a seguir.

```Rust
fn main() {
    let temperatura = 24.5;
    let velocidade = 10;
    println!("A temperatura é {:.2}°C e a velocidade é {}km/h", temperatura, velocidade);
}
```

Neste exemplo, usamos o formato `:.2` para limitar a saída da temperatura a dois números decimais após a vírgula. Isso é útil para impressão de valores numéricos com mais precisão. Também podemos usar um `:?` para imprimir valores de tipos de dados complexos, como estruturas ou vetores.

# Mergulho profundo:

Além dos formatos básicos como `{}` e `{:#}`, Rust também oferece uma macro `dbg!` para impressão de saída de depuração. Esta macro automaticamente imprime o valor e o nome da variável em que está sendo usado. Por exemplo:

```Rust
fn main() {
    let idade = 30;
    let nome = "João";
    dbg!(idade, nome);
}
```

Isso imprimiria:

```
[src/main.rs:4] idade = 30
[src/main.rs:5] nome = "João"
```

Isso pode ser muito útil quando estamos tentando rastrear bugs em programas maiores e mais complexos. Além disso, Rust também oferece ferramentas de depuração mais avançadas, como o uso do cargo com plataformas de integração contínua.

# Veja também:
- [Documentação oficial de Rust sobre impressão de saída de depuração](https://doc.rust-lang.org/std/macro.dbg.html)
- [Tutorial sobre impressão de saída de depuração em Rust](https://www.educative.io/blog/rust-debugging-print-macros)
- [Vídeo explicando técnicas de depuração em Rust](https://www.youtube.com/watch?v=TlDdbuu0oA4)