---
title:                "Concatenando strings"
aliases:
- /pt/ruby/concatenating-strings/
date:                  2024-01-20T17:35:37.531758-07:00
model:                 gpt-4-1106-preview
simple_title:         "Concatenando strings"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (O Que & Por Quê?)
Concatenar strings é o processo de juntar duas ou mais strings para formar uma única. Programadores fazem isso para construir mensagens, gerar saídas formatadas, ou simplesmente para organizar dados de forma legível.

## How to (Como Fazer):
```Ruby
# Usando o operador de soma (+)
saudacao = "Olá, " + "mundo!"
puts saudacao # => Olá, mundo!

# Usando o método concat
nome = "João"
nome.concat(" Silva")
puts nome # => João Silva

# Usando interpolação de strings com #{}
hora = "são " + "#{Time.now.hour} horas"
puts hora # Exemplo de saída: são 14 horas

# Usando o operador shovel (<<)
mensagem = "Ruby "
mensagem << "é "
mensagem << "incrível!"
puts mensagem # => Ruby é incrível!
```

## Deep Dive (Mergulho Profundo):
Concatenar strings é uma prática tão antiga quanto as próprias linguagens de programação. Historicamente, a necessidade surgiu porque as informações precisavam ser montadas em tempo de execução, frequentemente de pedaços de texto e variáveis.

No Ruby, realizar a concatenação diretamente com `+` cria um novo objeto a cada vez, o que pode ser ineficiente para múltiplas concatenações. O método `concat` ou o operador `<<` modifica a string original e é mais eficiente, pois não cria novos objetos.

Além disso, a interpolação de strings com `#{}` é uma maneira poderosa e preferível de inserir expressões dentro de strings, pois é mais legível e performática.

Vale dizer que métodos como `join` são usados quando se tem arrays de strings, o que oferece ainda outra alternativa para unir strings com eficiência e simplicidade.

## See Also (Veja Também):
- Documentação oficial do Ruby sobre strings: [Ruby-Doc String](https://ruby-doc.org/core-2.7.0/String.html)
- Ruby Style Guide, que inclui práticas recomendadas para interpolação de strings: [Ruby Style Guide](https://rubystyle.guide/#string-interpolation)
