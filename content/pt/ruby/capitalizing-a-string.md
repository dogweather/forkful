---
title:    "Ruby: Convertendo uma string em maiúsculas"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por que

Capitalizar uma string pode ser uma tarefa comum em muitos projetos de programação. Ao converter uma string para maiúsculas, podemos garantir que ela siga um padrão específico e seja facilmente legível por outros desenvolvedores que trabalham no mesmo código.

## Como Fazer

Para capitalizar uma string em Ruby, podemos utilizar o método `upcase`. Vamos dar uma olhada em um exemplo de código abaixo:

```Ruby
string = "hello world"
puts string.upcase
```

A saída deste código será `HELLO WORLD`, pois o `upcase` converte todas as letras da string em maiúsculas.

## Deep Dive

Além do método `upcase`, existem outras maneiras de capitalizar uma string em Ruby. Por exemplo, se quisermos que apenas a primeira letra da string seja maiúscula e o restante seja minúsculo, podemos usar o método `capitalize`. Vamos ver um exemplo:

```Ruby
string = "hello world"
puts string.capitalize
```

A saída será `Hello world`, com apenas a primeira letra maiúscula. Outro método útil é o `swapcase`, que inverte as letras maiúsculas e minúsculas em uma string. Vejamos um exemplo:

```Ruby
string = "hELLO wORLD"
puts string.swapcase
```

A saída será `Hello World`, com as letras maiúsculas se tornando minúsculas e vice-versa.

## Veja também

- [Documentação Ruby oficial sobre string](https://ruby-doc.org/core-2.7.1/String.html)
- [Tutorial sobre strings em Ruby](https://www.rubyguides.com/ruby-tutorial/string/)
- [Guia para iniciantes em Ruby](https://www.codecademy.com/learn/learn-ruby)