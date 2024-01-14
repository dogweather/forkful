---
title:    "Rust: Imprimindo saída de depuração"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Por que

Se você é um programador Rust iniciante ou experiente, é importante entender a importância de imprimir saídas de depuração (debug output). Essa é uma técnica crucial para identificar e resolver erros em seu código, tornando o processo de desenvolvimento mais eficiente e eficaz.

## Como Fazer

A impressão de saída de depuração do Rust é feita pela macro "println!", que funciona de maneira semelhante ao "printf" da linguagem C. Abaixo está um exemplo simples de como imprimir uma string na saída:

```Rust
fn main() {
    println!("Olá, mundo!");
}
```

Ao executar o código acima, a seguinte saída será exibida no console:

```
Olá, mundo!
```

Além de strings, é possível imprimir outros tipos de dados, como números inteiros e variáveis, utilizando a interpolação de strings:

```Rust
fn main() {
    let nome = "Maria";
    let idade = 25;

    println!("Meu nome é {} e eu tenho {} anos", nome, idade);
}
```

A saída será:

```
Meu nome é Maria e eu tenho 25 anos
```

Também é possível formatar a saída para melhor visualização. Por exemplo, se quisermos imprimir um número inteiro com três casas decimais, podemos fazer o seguinte:

```Rust
fn main() {
    let num = 5.4321;

    println!("O número é: {:.3}", num);
}
```

A saída será:

```
O número é: 5.432
```

## Mergulho Profundo

Além de imprimir saídas de depuração simples, o Rust oferece uma macro mais avançada para depuração: "dbg!". Essa macro imprime o valor de uma expressão e a própria expressão. Isso pode ser especialmente útil para lidar com erros em tempo de execução, onde você pode querer verificar o valor de uma variável antes de seu programa falhar.

Abaixo está um exemplo de como podemos usar o "dbg!" para imprimir a soma de dois números inteiros e a expressão inteira:

```Rust
fn main() {
    let num1 = 10;
    let num2 = 20;

    dbg!(num1 + num2);
}
```

A saída será:

```
[ze-dbg] (src/main.rs:5) num1 + num2 = 30
```

Além disso, existem outras macros úteis para depuração, como "eprintln!", que imprime saída de erro e "write!", que escreve em uma determinada stream de saída.

## Veja Também

- [Documentação oficial do Rust para impressão de saída de depuração](https://doc.rust-lang.org/std/macro.println.html)
- [Artigo sobre depuração em Rust da Rust by Example](https://doc.rust-lang.org/rust-by-example/hello/print/print_debug.html)
- [Vídeo tutorial sobre depuração em Rust da Rustacean Station](https://www.youtube.com/watch?v=NWw7vGLcRfw)