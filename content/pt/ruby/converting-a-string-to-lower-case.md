---
title:    "Ruby: Convertendo uma string para letra minúscula"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por que converter uma string para minúsculas?

Ao trabalhar com strings em Ruby, muitas vezes é necessário padronizar o texto para facilitar a manipulação e comparação. Converter uma string para minúsculas é uma das formas de padronizar o texto e garantir que não haja diferença entre as letras maiúsculas e minúsculas.

## Como converter uma string para minúsculas

```Ruby
texto = "Olá, Mundo!"
puts texto.downcase
```

Output:
```
olá, mundo!
```

Para converter uma string para minúsculas, podemos utilizar o método `downcase`, que transforma todas as letras maiúsculas em minúsculas. Podemos atribuir o resultado a uma variável ou simplesmente imprimir na tela.

Outra opção seria utilizar o método `downcase!`, que modifica a própria string em vez de retornar uma cópia modificada. Por exemplo:

```Ruby
texto = "Olá, Mundo!"
puts texto.downcase!
puts texto
```

Output:
```
olá, mundo!
olá, mundo!
```

Observe que agora a própria variável `texto` foi modificada.

## Deep Dive

Ao converter uma string para minúsculas, é importante ter em mente que alguns caracteres especiais podem ser afetados. Por exemplo, em português, a letra "ç" fica em minúsculo como "ç", enquanto em maiúsculo é "Ç". Por isso, é necessário utilizar métodos mais especializados caso precise manter esses caracteres especiais no texto.

Além disso, também é possível utilizar o método `downcase!` em uma string vazia (`""`) sem gerar erros. Isso porque a própria string vazia não possui letras para serem convertidas e, portanto, permanece vazia após a chamada do método.

## Veja também

- [Documentação oficial do método `downcase`](https://ruby-doc.org/core-2.7.1/String.html#method-i-downcase)
- [Tutorial sobre strings em Ruby](https://www.tutorialspoint.com/ruby/ruby_strings.htm)
- [Outros métodos para manipular strings em Ruby](https://www.rubyguides.com/2019/02/ruby-strings/)