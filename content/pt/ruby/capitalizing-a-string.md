---
title:                "Capitalizando uma string"
html_title:           "Ruby: Capitalizando uma string"
simple_title:         "Capitalizando uma string"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# O que e porque?

Capitalizar uma string significa colocar a primeira letra de cada palavra em maiúsculo. Programadores geralmente fazem isso para seguir as convenções de formatação de código e para tornar o texto mais legível.

# Como fazer:

```
# Exemplo 1:
puts "ruby é uma linguagem incrível".capitalize
# Saída: "Ruby é uma linguagem incrível"

# Exemplo 2:
string = "estou aprendendo a programar em ruby"
puts string.capitalize
# Saída: "Estou aprendendo a programar em ruby"

# Exemplo 3:
puts "lychee".capitalize
# Saída: "Lychee"
```

# Mergulho Profundo:

A prática de capitalização de strings pode ser vista em muitas linguagens de programação, incluindo Ruby, para manter um padrão consistente de formatação. Alternativas incluem o uso de métodos como `upcase` e `downcase`, que alteram todas as letras da string para maiúsculas ou minúsculas, respectivamente. A implementação do método `capitalize` em Ruby se baseia no uso de expressões regulares para encontrar os limites de cada palavra e transformar a primeira letra em maiúscula.

# Veja também:

- Documentação oficial da linguagem Ruby: https://www.ruby-lang.org/pt/
- Stack Overflow: https://stackoverflow.com/questions/18386618/how-to-capitalize-a-string-in-ruby