---
title:    "Rust: Eliminando caracteres que correspondem a um padrão"
keywords: ["Rust"]
---

{{< edit_this_page >}}

# Por que excluir caracteres correspondentes a um padrão?

Muitas vezes, ao trabalhar com strings em um programa de Rust, pode ser necessário excluir certos caracteres que correspondem a um padrão específico. Isso pode ser útil em várias situações, como limpar dados de entrada, realizar operações de busca e substituição, ou apenas para simplificar e manipular strings. Neste post, vamos explorar como podemos fazer isso de forma eficiente em Rust.

## Como fazer
A primeira coisa que precisamos fazer para excluir caracteres correspondentes a um padrão é importar o pacote `regex`. Isso nos permitirá utilizar expressões regulares para encontrar o padrão desejado em uma string.

```
use regex::Regex;
```

Agora, podemos criar nossa string na qual queremos basear nossa busca. Para este exemplo, vamos utilizar a frase "Eu amo programar em Rust!" e excluir todos os caracteres que correspondem ao padrão "a" ou "A".

```
let frase = "Eu amo programar em Rust!";
```

Em seguida, utilizaremos a função `split` da biblioteca `regex` para particionar nossa string em todas as ocorrências do padrão especificado, que neste caso são "a" ou "A". Então, podemos usar o método `filter` para filtrar todos os caracteres correspondentes à nossa string original.

```
let re = Regex::new("[aA]").unwrap();
let resultado = re.split(frase).into_iter().filter(|x| !x.is_empty()).collect::<Vec<_>>();
```

E para ver o resultado final, usaremos um simples comando `print`, que nos dará uma saída sem os caracteres correspondentes ao padrão.

```
println!("{:?}", resultado);
```

A saída será o seguinte vetor:

```
["Eu ", "mo progrmr em Rust!"];
```

## Aprofundando 
Existem algumas coisas importantes a serem lembradas ao excluirmos caracteres correspondentes a um padrão em Rust. Primeiro, devemos considerar as diferenças entre letras maiúsculas e minúsculas ao utilizar um padrão específico. Em nosso exemplo, utilizamos "[aA]", o que significa que todos os caracteres "a" e "A" serão excluídos. No entanto, se quisermos excluir apenas caracteres maiúsculos, teríamos que usar "[A]" em seu lugar.

Além disso, é importante notar que ao utilizar o método `split` da biblioteca `regex`, devemos lembrar de usar o método `filter` para remover quaisquer strings vazias que possam ser geradas pelo padrão especificado. Isso garantirá que nosso resultado final seja uma string limpa e sem caracteres indesejados.

## Veja também
- [The Rust Programming Language](https://www.rust-lang.org/)
- [Regex Crate Documentation](https://docs.rs/regex/1.4.2/regex/)