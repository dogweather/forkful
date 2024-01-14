---
title:    "Ruby: Removendo caracteres que correspondem a um padrão"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Por que

Você provavelmente já se deparou com a necessidade de excluir caracteres que correspondem a um determinado padrão ao escrever código Ruby. Talvez você tenha recebido dados de uma API com informações desnecessárias ou precise filtrar uma string antes de manipulá-la. Independentemente do motivo, saber como fazer isso pode ser muito útil em suas habilidades de programação.

## Como fazer

Usando Ruby, há várias maneiras de excluir caracteres que correspondem a um padrão. Aqui estão alguns exemplos usando expressões regulares e o método `gsub`.

```Ruby
string = "Apaga caracteres!999"
nova_string = string.gsub(/[0-9]/, "")
puts nova_string # Saída: Apaga caracteres!
```

```Ruby
string = "Olá mundo!"
nova_string = string.gsub(/[aeiou]/, "")
puts nova_string # Saída: Ol mnd! 
```

```Ruby
string = "Ruby é demais!"
nova_string = string.gsub(/[^a-zA-Z]/, "")
puts nova_string # Saída: Rubydemais
```

Você também pode usar o método `delete` para excluir caracteres semelhantes a `gsub`.

```Ruby
string = "Ruby é incrível!"
nova_string = string.delete("aeiou")
puts nova_string # Saída: Rby crv!
```

## Mergulho Profundo

Ao usar expressões regulares para excluir caracteres, é importante entender os metacaracteres que podem ser usados nessa situação. Alguns dos principais metacaracteres incluem `.` para qualquer caractere, `+` para um ou mais ocorrências do caractere anterior, e `[^]` para negar um conjunto de caracteres.

Além disso, é útil saber que o método `gsub` pode aceitar outra string como argumento, substituindo os caracteres que correspondem por essa string.

Veja mais exemplos do método `gsub` abaixo.

```Ruby
string = "Olá mundo!"
nova_string = string.gsub(/[a-z]/, "*")
puts nova_string # Saída: **á **ú**o!
```

```Ruby
string = "Bem-vindo!"
nova_string = string.gsub(/(-)|(!)/, "")
puts nova_string # Saída: Bemvindo
```

```Ruby
string = "Ruby é incrível!"
nova_string = string.gsub(/!/, " não")
puts nova_string # Saída: Ruby é incrível não
```

## Veja também

- [Documentação oficial do Ruby para o método `gsub`](https://ruby-doc.org/core-2.7.1/String.html#method-i-gsub)
- [Exemplo de uso do método `gsub`](https://www.geeksforgeeks.org/ruby-string-gsub-method-with-example/)
- [Tutorial sobre expressões regulares em Ruby](https://www.rubyguides.com/2015/06/ruby-regex/)