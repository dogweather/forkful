---
title:                "Ruby: Excluindo caracteres que correspondem a um padrão"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que

Você já se deparou com a necessidade de excluir caracteres que correspondem a um determinado padrão ao trabalhar com código Ruby? Talvez você precise remover acentos de uma string ou filtrar símbolos especiais de um texto. Nesses casos, a remoção de caracteres que correspondem a um padrão pode ser muito útil. Neste artigo, vamos explorar como fazer isso de forma eficiente em Ruby.

## Como Fazer

Existem várias maneiras de excluir caracteres que correspondem a um padrão em Ruby, mas uma das mais simples é usando os métodos `gsub` ou `delete`. Ambos os métodos recebem um padrão como argumento e substituem ou excluem os caracteres correspondentes da string original.

Vamos ver um exemplo de uso do método `gsub` para remover acentos de uma string:

```Ruby
# Exemplo de uso do gsub para remover acentos
frase = "Olá, meu nome é João"
frase_modificada = frase.gsub(/[áàãâä]/, "a").gsub(/[éèêë]/, "e").gsub(/[íìîï]/, "i").gsub(/[óòõôö]/, "o").gsub(/[úùûü]/, "u")
puts frase # "Olá, meu nome é João"
puts frase_modificada # "Ola, meu nome e Joao"
```

No exemplo acima, usamos uma expressão regular para identificar os acentos e substituí-los pelas letras correspondentes sem acento. Dessa forma, conseguimos ter uma string sem acentos que ainda preserva o sentido original da frase.

Também é possível usar o método `delete` para excluir caracteres que correspondem a um determinado padrão. Veja um exemplo:

```Ruby
# Exemplo de uso do delete para filtrar símbolos especiais
texto = "Este é um texto ã um @# exemplo!"
texto_filtrado = texto.delete("^a-zA-Z0-9")
puts texto # "Este é um texto ã um @# exemplo!"
puts texto_filtrado # "Este é um texto um exemplo"
```

No exemplo acima, usamos o padrão `^a-zA-Z0-9`, que remove todos os caracteres exceto letras e números, para filtrar símbolos especiais.

## Deep Dive

Se você quiser se aprofundar mais em como os métodos `gsub` e `delete` funcionam, pode dar uma olhada na documentação oficial do Ruby ou em tutoriais mais detalhados sobre expressões regulares. Além disso, também é possível usar outras ferramentas como o método `gsub!` para substituir os caracteres diretamente na string original.

Em alguns casos, também pode ser necessário manipular a string antes de aplicar esses métodos, como convertê-la para minúsculas ou remover espaços em branco. Portanto, é importante entender bem os métodos disponíveis e como eles podem ser utilizados de forma combinada para obter o resultado desejado.

## See Also

Caso queira se aprofundar mais em expressões regulares e manipulação de strings em Ruby, confira os links abaixo:

- [Documentação oficial do Ruby sobre o método `gsub`](https://ruby-doc.org/core-2.7.1/String.html#method-i-gsub)
- [Tutorial sobre expressões regulares em Ruby](https://www.rubyguides.com/2015/06/ruby-regex/)
- [Vídeo explicando o uso do método `gsub`](https://www.youtube.com/watch?v=MLEg5bD4mzk)

Agora que você já sabe como excluir caracteres que correspondem a um padrão em Ruby, aproveite essa ferramenta para tornar seu código ainda mais eficiente e legível. Até a próxima!