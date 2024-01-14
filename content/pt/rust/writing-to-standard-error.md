---
title:                "Rust: Escribir em erros padrão"
programming_language: "Rust"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que escrever para a saída de erro padrão em Rust?

Se você é um programador Rust, provavelmente já ouviu falar sobre a importância de escrever para a saída de erro padrão. Mas você pode estar se perguntando: por que é tão importante? A resposta é simples: escrever para a saída de erro padrão é uma forma eficaz de lidar com erros e depurar seu código.

## Como fazer?

Escrever para a saída de erro padrão em Rust é muito simples e direto. Você só precisa usar a macro "eprintln!" para imprimir seu erro na saída de erro padrão. Esta é a forma básica de fazer isso:

```Rust
fn main() {
  if 1 + 1 == 3 {
    eprintln!("Algo de errado não está certo!");
  }
}
```

Agora, se você quer ser mais específico e adicionar informações adicionais ao seu erro, você pode usar a macro "format!" dentro da macro "eprintln!". Por exemplo:

```Rust
fn main() {
  let num1 = 1;
  let num2 = 0;
  if num2 == 0 {
    eprintln!("Erro: Divisão por zero! Números fornecidos: {} e {}", num1, num2);
  }
}
```

## Profundidade

Agora que você sabe como escrever para a saída de erro padrão em Rust, vamos dar uma olhada mais profunda no porquê de ser tão importante. A saída de erro padrão é especialmente útil quando você está lidando com erros imprevisíveis ou difíceis de rastrear. Ao escrever para a saída de erro padrão, você pode ver exatamente o que aconteceu quando o erro ocorreu e seguir o rastro até sua origem. Isso economiza muito tempo e esforço na depuração de código.

Outra vantagem de escrever para a saída de erro padrão é que você pode usar essa informação para melhorar ainda mais seu código. Ao identificar e corrigir erros, você pode construir um código mais confiável e robusto.

## Veja também

Para saber mais sobre a escrita para a saída de erro padrão em Rust, confira os seguintes links:

- https://www.rust-lang.org/learn (em inglês)
- https://doc.rust-lang.org/book/ (em inglês)
- https://pt.wikipedia.org/wiki/Rust_(linguagem_de_programa%C3%A7%C3%A3o) (em português)