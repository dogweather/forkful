---
title:    "Ruby: Removendo caracteres que correspondem a um padrão"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que

Manipulação de strings é uma parte fundamental da programação e às vezes precisamos lidar com caracteres indesejados dentro de uma string. Para isso, a linguagem Ruby nos oferece uma maneira eficiente de deletar caracteres que correspondem a um padrão específico.

## Como fazer

Em Ruby, podemos usar o método `gsub` para substituir caracteres correspondentes a um padrão por uma string vazia. Veja um exemplo abaixo:

```Ruby
string = "Olá! Meu nome é João."

p string.gsub(/[a-zA-Z]/, '')
# output: "! ."

p string.gsub(/[A-Z]/, '')
# output: "lá! eu nome é oão."
```
Neste exemplo, usamos o padrão `[a-zA-Z]` para encontrar e deletar todas as letras de A a Z, resultando em `! .` como o primeiro output e `lá! eu nome é oão.` como o segundo output.

Também é possível usar o método `delete` para eliminar caracteres de uma string com base em um padrão. Veja outro exemplo:

```Ruby
string = "123,456,789"

p string.delete(",")
# output: "123456789"

p string.delete("0-9")
# output: ",,"
```
Neste caso, usamos o padrão `,` para remover todas as ocorrências de vírgulas e o padrão `0-9` para apagar todos os números da string.

## Mergulho Profundo

Além dos exemplos acima, o Ruby oferece muitas outras maneiras de deletar caracteres que correspondem a um padrão. Por exemplo, podemos usar os métodos `slice` e `slice!` para cortar uma string e retornar uma substring sem os caracteres correspondentes ao padrão. Também podemos usar a classe `StringScanner` para fazer um scan em uma string e remover as letras de A a Z.

É importante lembrar que o Ruby é uma linguagem fortemente tipada, o que significa que os métodos de manipulação de strings mencionados aqui são muito seguros e confiáveis. No entanto, devemos sempre tomar cuidado ao manipular dados e garantir que nossos códigos estejam tratando exceções adequadamente.

## Veja também

Aqui estão alguns links para mais informações sobre como deletar caracteres correspondentes a um padrão em Ruby:

- [documentação do método `gsub` na Ruby API](https://ruby-doc.org/core-2.7.1/String.html#method-i-gsub)
- [documentação do método `delete` na Ruby API](https://ruby-doc.org/core-2.7.1/String.html#method-i-delete)
- [documentação da classe `StringScanner` na Ruby API](https://ruby-doc.org/stdlib-2.7.1/libdoc/stringio/rdoc/StringScanner.html)