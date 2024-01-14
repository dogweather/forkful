---
title:    "Rust: Convertendo uma string para letra minúscula."
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por que

Quando escrevemos programas em Rust, é importante considerar a usabilidade para o usuário final. Uma das tarefas comuns em programação é formatar strings, e em geral, é mais desejável que elas estejam em letras minúsculas para uma melhor visualização e organização. Neste artigo, vamos explorar como converter uma string para minúsculas em Rust.

## Como Fazer

O processo de conversão de uma string para minúsculas em Rust é bastante simples. Primeiro, importamos o módulo "string" e usamos o método "to_lowercase()". Vamos ver um exemplo:

```Rust
use std::string;

let frase = String::from("OLá MuNDo!");

let resultado = frase.to_lowercase();
println!("{}", resultado);
```

Neste exemplo, importamos o módulo "string" e criamos uma string contendo a frase "OLá MuNDo!". Em seguida, usamos o método "to_lowercase()" para converter a string para letras minúsculas e a imprimimos na tela. O resultado será "olá mundo!".

## Uma Profundidade Maior

Por baixo dos panos, o método "to_lowercase()" usa a função "chars()" para iterar através de cada caractere da string e aplicar a função "to_lowercase()" individualmente para convertê-los. Isso permite que ele lide com caracteres acentuados e símbolos especiais de forma correta e eficiente. No entanto, é importante lembrar que o resultado final será uma nova string e a string original não será modificada.

## Veja Também
 
 - [Documentação do Módulo "string" em Rust](https://doc.rust-lang.org/std/string/index.html)
 - [Explicação Detalhada do Método "to_lowercase()" no Rust Cookbook](https://rust-lang-nursery.github.io/rust-cookbook/text/strings.html#convert-a-case-of-a-string)
 - [Vídeo Tutorial sobre Conversão de Strings no Rust](https://www.youtube.com/watch?v=Qezm_KpOits)