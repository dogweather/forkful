---
title:    "Ruby: Buscando e substituindo texto"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Por que

Se você é um programador em Ruby, provavelmente já encontrou a necessidade de procurar e substituir trechos de texto em seus códigos. Talvez para corrigir um erro ortográfico, ou até mesmo para refatorar uma longa lista de variáveis com nomes sem sentido. Independente do motivo, procurar e substituir é uma tarefa bastante comum na programação e pode economizar muito tempo e esforço.

## Como Fazer

A busca e substituição de texto no Ruby é simples e pode ser realizada usando o método `gsub` (substituir globalmente) de uma string. Veja um exemplo abaixo mostrando como substituir a palavra "vermelho" por "azul" em uma lista de cores:

```Ruby
cores = "vermelho, verde, amarelo, vermelho, laranja"
puts cores.gsub("vermelho", "azul")
```

O resultado será: `azul, verde, amarelo, azul, laranja`

Você também pode usar expressões regulares em conjunto com o método `gsub` para fazer substituições mais específicas. Por exemplo, suponha que você tenha uma lista de nomes separados por vírgulas e deseja substituir apenas os nomes que começam com a letra "A" por "Ana". Veja como isso pode ser feito:

```Ruby
nomes = "Alice, Bruno, Carolina, Amanda, Renato"
puts nomes.gsub(/^A/, "Ana")
```

O resultado será: `Ana, Bruno, Carolina, Ana, Renato`

Note que usamos a expressão regular `^A` para encontrar apenas os nomes que começam com "A" e substituí-los por "Ana".

## Profundidade

Agora que você já sabe como fazer busca e substituição em seus códigos Ruby, vamos mergulhar um pouco mais nesse assunto. O método `gsub` é uma abreviação para "global substitution", o que significa que ele substitui todas as ocorrências do padrão especificado em uma string. No entanto, se você quiser substituir apenas a primeira ocorrência, pode usar o método `sub` (substituir). Veja um exemplo abaixo:

```Ruby
frase = "O gato sempre olha para o céu"
puts frase.sub("para", "pela")
```

O resultado será: `O gato sempre olha pela céu`

Outra coisa importante a saber é que você pode usar variáveis ou blocos em seus substitutos. Isso permite que você faça substituições mais dinâmicas e até mesmo use lógica em suas substituições. Por exemplo:

```Ruby
cachorro = "labrador"
puts cachorro.gsub("labrador", "golden retriever") { |c| "O cachorro é um #{c}" }
```

O resultado será: `O cachorro é um golden retriever`

Aqui, usamos o bloco para adicionar a frase "O cachorro é um" antes da substituição, deixando a frase mais completa.

## Veja Também

- [Documentação do método gsub no RubyDocs](https://ruby-doc.org/core-2.6/String.html#method-i-gsub)
- [Guia de expressões regulares no Ruby](https://www.regular-expressions.info/ruby.html)
- [Canal do YouTube "The Net Ninja" com tutoriais sobre Ruby](https://www.youtube.com/channel/UCW5YeuERMmlnqo4oq8vwUpg)