---
title:                "Encontrando o comprimento de uma string"
html_title:           "C: Encontrando o comprimento de uma string"
simple_title:         "Encontrando o comprimento de uma string"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## O Que & Por Que?

Encontrar o comprimento de uma string, em programação, significa determinar o número de caracteres na string. Os programadores fazem isso para manipular e gerenciar conteúdo textual de forma eficaz em suas aplicações.

## Como Fazer:

No Ruby, para encontrar o comprimento de uma string, usamos o método `length` ou `size`. Aqui estão alguns exemplos:

```Ruby
exemplo = "Olá, mundo!"
puts exemplo.length  # Saída: 12
puts exemplo.size    # Saída: 12
```

Observe que ambos os métodos `length` e `size` retornam o mesmo resultado.

## Imersão Profunda:

- **Contexto histórico**: Ruby, desde o início, fornece métodos para encontrar o comprimento de uma string. O motivo desses métodos existirem é facilitar as operações com strings para os desenvolvedores.

- **Alternativas**: Embora `length` e `size` sejam os mais comumente usados, há outro método que você pode usar, o `bytesize`. Ele retorna o comprimento da string em bytes, o que pode ser diferente do número de caracteres devido à codificação.
  
  ```Ruby
  exemplo = "Olá, mundo!"
  puts exemplo.bytesize  # Saída: 14
  ```

- **Detalhes de implementação**: Por baixo dos panos, quando você chama o método `length` ou `size` numa string, Ruby conta o número de caracteres na string, levando em consideração a codificação da string.

## Veja Também:

- [Documentação oficial do Ruby sobre strings](https://ruby-doc.org/core-2.7.0/String.html)
- [Tutorial detalhado sobre strings em Ruby](https://www.tutorialspoint.com/ruby/ruby_strings.htm)
- [Mais detalhes sobre metodologias de codificação em Ruby](http://ruby-for-beginners.rubymonstas.org/built_in_classes/strings.html)