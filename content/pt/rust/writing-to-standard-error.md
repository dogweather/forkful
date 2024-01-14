---
title:    "Rust: Escrevendo para o erro padrão"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que escrever para o Standard Error em Rust

Se você é um desenvolvedor Rust, pode se perguntar por que precisaria escrever para o standard error em seu código. A resposta é simples: escrever para o standard error permite que você imprima mensagens de erro e informações de depuração no terminal enquanto seu código está sendo executado. Isso pode ser útil para identificar e solucionar problemas em seu código.

## Como escrever para o Standard Error em Rust

Escrever para o standard error em Rust é simples e direto. Você só precisa usar a função `eprintln!` do módulo `std`, seguida da mensagem que deseja imprimir. Por exemplo:

```Rust
eprintln!("Erro: divisão por zero!");
```

Dessa forma, a mensagem será impressa no terminal sempre que esse trecho de código for executado. Você também pode utilizar a macro `dbg!` para imprimir informações de depuração, como valores de variáveis. Por exemplo:

```Rust
let num1 = 10;
let num2 = 0;
dbg!(num1 / num2);
```

Isso resultaria na seguinte saída:

```bash
[thread 'main' panicked at 'attempt to divide by zero', src/main.rs:4:23]
```

## Mergulho Profundo: Mais informações sobre escrever para o Standard Error

Ao utilizar a função `eprintln!`, você também pode especificar outros argumentos, como estilo de formatação e variáveis para serem impressas. Além disso, você também pode utilizar a função `set_panic_hook` do módulo `std::panic` para registrar uma função que será executada em caso de pânico no seu código. Isso pode ser útil para capturar e imprimir informações de depuração em caso de erros inesperados.

## Ver também

Aqui estão alguns links úteis para saber mais sobre como escrever para o standard error em Rust:

- Documentação oficial do Rust sobre a função `eprintln!`: https://doc.rust-lang.org/std/macro.eprintln.html
- Tutorial da Rust Programming Language sobre como imprimir informações de depuração: https://doc.rust-lang.org/book/ch09-01-unrecoverable-errors-with-panic.html
- Vídeo tutorial sobre como depurar seu código Rust utilizando `dbg!`: https://www.youtube.com/watch?v=x9-OomMz3CA