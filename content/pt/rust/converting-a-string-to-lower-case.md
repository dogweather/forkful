---
title:    "Rust: Convertendo uma string para minúsculas"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Por que converter uma string para letras minúsculas?

Ao trabalhar com strings em um programa, pode ser necessário converter todas as letras para minúsculas. Isso pode ser útil para manipulação de dados, comparação de strings ou formatação de saída. Felizmente, Rust torna isso muito simples com sua função integrada `to_lowercase()`. Vamos ver como fazer isso!

## Como fazer a conversão em Rust

Para converter uma string para letras minúsculas em Rust, basta chamar a função `to_lowercase()` na string desejada. Aqui está um exemplo:

```Rust
fn main() {
    let minha_string = "Olá, Mundo!";
    println!("{}", minha_string.to_lowercase()); // imprime "olá, mundo!"
}
```

É importante notar que a função `to_lowercase()` retorna uma nova string com as letras em minúsculo, a string original permanecerá inalterada. Agora, se quisermos salvar a nova string em uma variável, podemos fazer assim:

```Rust
fn main() {
    let minha_string = "Olá, Mundo!";
    let nova_string = minha_string.to_lowercase();
    println!("{}", nova_string); // imprime "olá, mundo!"
}
```

Podemos até mesmo trabalhar com strings que são passadas como argumentos em funções. Por exemplo:

```Rust
fn imprimir_string_em_minusculas(string: String) {
    let string_minuscula = string.to_lowercase();
    println!("{}", string_minuscula); // imprime a string em minúsculo
}

fn main() {
    let minha_string = "Rust é incrível!".to_string();
    imprimir_string_em_minusculas(minha_string); // imprime "rust é incrível!"
}
```

## Mergulhando mais fundo

Em uma olhada rápida, o método `to_lowercase()` parece ser bastante direto. Mas, por baixo dos panos, há muito mais acontecendo. Por exemplo, ele não apenas substitui as letras maiúsculas por minúsculas, mas também leva em consideração questões de localização, ou seja, dependendo do idioma, algumas letras maiúsculas podem ser convertidas para mais de uma letra minúscula.

Além disso, o método `to_lowercase()` também funciona com símbolos não alfabéticos, como números e pontuação.

É importante notar que a função `to_lowercase()` pode não funcionar como esperado se o local em que o código está sendo executado tiver um conjunto de regras de caixa diferente do inglês. Para evitar problemas, é possível usar a função `to_lowercase()` em conjunção com a função `chars()` para garantir que todas as letras sejam convertidas corretamente.

## Veja também

- [Documentação oficial do Rust sobre a função to_lowercase()](https://doc.rust-lang.org/std/string/struct.String.html#method.to_lowercase)
- [Tutorial sobre strings em Rust](https://www.tutorialspoint.com/rust/rust_strings.htm)
- [Exemplos práticos de conversão de strings em Rust](https://www.geeksforgeeks.org/working-with-strings-in-rust/)