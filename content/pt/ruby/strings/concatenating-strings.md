---
date: 2024-01-20 17:35:37.531758-07:00
description: "Concatenar strings \xE9 o processo de juntar duas ou mais strings para\
  \ formar uma \xFAnica. Programadores fazem isso para construir mensagens, gerar\
  \ sa\xEDdas\u2026"
lastmod: '2024-03-11T00:14:20.829403-06:00'
model: gpt-4-1106-preview
summary: "Concatenar strings \xE9 o processo de juntar duas ou mais strings para formar\
  \ uma \xFAnica. Programadores fazem isso para construir mensagens, gerar sa\xED\
  das\u2026"
title: Concatenando strings
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
