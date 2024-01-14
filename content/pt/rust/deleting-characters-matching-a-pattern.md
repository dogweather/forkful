---
title:                "Rust: Excluindo caracteres que correspondem a um padrão."
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que

Existe uma tarefa comum de edição de texto que é deletar caracteres em um determinado padrão. Essa ação pode ser realizada manualmente, mas em linguagens de programação como o Rust, existem maneiras mais eficientes de lidar com o problema.

## Como fazer

Para começar, vamos definir nosso texto de exemplo e o padrão que queremos deletar:

```Rust
let texto = "Esta é uma string de exemplo que contém alguns caracteres desnecessários.";
let padrao = "a"; // vamos deletar todas as letras "a" do texto
```

Para deletar os caracteres que correspondem ao padrão, podemos usar o método `replace` da biblioteca padrão do Rust:

```Rust 
let novo_texto = texto.replace(padrao, "");
println!("{}", novo_texto);
```

Isso imprimirá o novo texto sem os caracteres correspondentes ao padrão:

```
Est é um string de exemplo que contém alguns caracteres desnecessários.
```

Se o seu texto é mutável, é possível usar o método `retain` para modificar o texto atual:

```Rust 
texto.retain(|c| c != 'a');
println!("{}", texto);
```

## Deep Dive

No código acima, usamos a função de ordenação `retain` do Rust, que filtra nossos caracteres com base em uma condição. No nosso caso, queríamos manter apenas os caracteres que não correspondiam ao padrão que queríamos deletar.

É importante notar que o método `replace` é mais indicado para textos pequenos, pois ele aloca uma nova `String` para o novo texto. Já o método `retain` modifica diretamente o texto existente, o que pode ser mais eficiente para textos maiores.

Outra opção para deletar caracteres em um padrão é usar regex (expressões regulares), que permitem que você faça buscas mais complexas em um texto. O Rust possui uma biblioteca `regex` que pode ser usada para isso.

## Veja também
- [Documentação da biblioteca padrão do Rust](https://doc.rust-lang.org/std)
- [Tutorial de regex no Rust](https://blog.mediapart.fr/howard-guinaldo/blog/121017/rust-regex-tutorial)
- [Referência das funções `replace` e `retain`](https://doc.rust-lang.org/std/string/index.html#method.retain)