---
title:                "Rust: Extraindo subcadeias"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que extrair subcadeias

Extrair subcadeias, ou substrings, é uma tarefa comum em programação. Isso geralmente é necessário quando queremos separar uma parte específica de uma string maior para manipulá-la ou usá-la em outro contexto. Em Rust, existem várias maneiras de extrair substrings de uma string, dependendo das necessidades do seu programa.

## Como fazer

A primeira maneira de extrair substrings em Rust é usando o método `split()` da struct `str`. Este método retorna um `Split` iterator, que pode ser iterado usando o método `next()` para obter as substrings desejadas. Por exemplo:

```Rust
let s = "Esta é uma string de exemplo";
let mut splitted = s.split(" ");
println!("As substrings são: ");
for substring in splitted {
    println!("{}", substring);
}
```

A saída deste código será:

```
As substrings são:
Esta
é
uma
string
de
exemplo 
```

Outra maneira de extrair substrings é usando os métodos `get()` ou `slice()` da struct `str`. O método `get()` retorna uma referência à substring em uma determinada posição, enquanto o método `slice()` retorna uma cópia da substring. Por exemplo:

```Rust
let s = "Exemplo";
let substring = &s[0..3]; // obtém os primeiros três caracteres
println!("A substring é: {}", substring);
```

A saída deste código será:

```
A substring é: Exe
```

Ambos os métodos também podem ser usados com `Range` e `RangeFrom` para extrair substrings com base em intervalos. Por exemplo:

```Rust
let s = "Exemplo";
let substring = &s[3..]; // obtém os últimos quatro caracteres
println!("A substring é: {}", substring);
```

A saída deste código será:

```
A substring é: plo
```

Finalmente, se você precisar de mais controle ao extrair substrings, pode usar o método `chars()` da struct `str` para obter um iterador de caracteres. Isso permite que você acesse cada caractere individualmente e, a partir daí, construa a substring desejada. Por exemplo:

```Rust
let s = "Exemplo";
let characters = s.chars();
let mut substring = String::new();
// obtém os dois últimos caracteres
for _ in 0..2 {
    substring.push(characters.next().unwrap());
}
println!("A substring é: {}", substring);
```

A saída deste código será:

```
A substring é: lo
```

## Mergulho profundo

Vale ressaltar que as operações de substring em Rust são `O(len)` devido à codificação das strings na memória. Isso pode causar problemas de desempenho em strings muito grandes ou em situações em que a extração de substrings é feita repetidamente em um loop. Nesses casos, é recomendável considerar o uso de uma implementação de string que armazena os índices de início e fim de cada caractere em vez de codificá-los em bytes. Além disso, antes de extrair substrings, sempre verifique se os índices estão dentro dos limites da string para evitar erros.

## Veja também

- [Documentação oficial do Rust sobre strings](https://doc.rust-lang.org/std/string/struct.String.html)
- [Tutorial de uso de strings em Rust](https://brson.github.io/rust-cookbook/text/strings.html)
- [Exemplo de split e concatenação de strings em Rust](https://www.rust-lang.org/pt-BR/learn/whats-new/rust-1-56#split-and-concatenation-of-strings)