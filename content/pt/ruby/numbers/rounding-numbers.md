---
aliases:
- /pt/ruby/rounding-numbers/
date: 2024-01-26 03:46:46.704963-07:00
description: "Arredondar n\xFAmeros significa ajust\xE1-los ao n\xFAmero inteiro mais\
  \ pr\xF3ximo ou a um grau de precis\xE3o especificado. Programadores arredondam\
  \ n\xFAmeros para\u2026"
lastmod: 2024-02-18 23:08:58.658648
model: gpt-4-0125-preview
summary: "Arredondar n\xFAmeros significa ajust\xE1-los ao n\xFAmero inteiro mais\
  \ pr\xF3ximo ou a um grau de precis\xE3o especificado. Programadores arredondam\
  \ n\xFAmeros para\u2026"
title: "Arredondamento de n\xFAmeros"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Arredondar números significa ajustá-los ao número inteiro mais próximo ou a um grau de precisão especificado. Programadores arredondam números para simplificar, para corresponder às expectativas humanas, ou para adaptar dados a formatos específicos — pense em cálculos financeiros, displays gráficos ou redução do tamanho de armazenamento.

## Como Fazer:

```Ruby
# Arredondamento básico
puts 3.14159.round      # => 3
puts 2.6.round          # => 3

# Especificando a precisão
puts 3.14159.round(2)   # => 3.14
puts 2.675.round(2)     # => 2.68

# Arredondando para baixo
puts 2.9.floor          # => 2

# Arredondando para cima
puts 2.1.ceil           # => 3

# Arredondando em direção a zero
puts -2.9.round         # => -3
puts -2.9.truncate      # => -2
```

Saída de Amostra:
```
3
3
3.14
2.68
2
3
-3
-2
```

## Aprofundamento
Arredondar números não é uma novidade — os humanos têm feito isso por séculos para facilitar cálculos ou para trabalhar dentro dos limites de suas ferramentas. Em Ruby, o método `round` é versátil, com a capacidade de arredondar para o número inteiro mais próximo por padrão ou para um lugar decimal especificado.

Uma alternativa ao `round` é `floor` para sempre arredondar para baixo, e `ceil` para sempre arredondar para cima, independentemente do valor do número. Para simplesmente cortar os lugares decimais, você tem o `truncate`.

Historicamente, quando se trata de computadores, arredondar torna-se crítico ao lidar com aritmética de ponto flutuante devido à sua imprecisão inerente. Ruby, como a maioria das linguagens, segue o padrão IEEE 754 para números de ponto flutuante, o que significa que ele lida com o arredondamento de uma maneira que a maioria dos programadores deve conseguir prever e confiar.

Mas há mais nisso tudo — coisas como o arredondamento do banqueiro (também conhecido como arredondar para o par mais próximo) são conceitos que desenvolvedores Ruby podem precisar implementar manualmente, já que o método `round` não o oferece de forma direta.

## Veja Também
- A [Documentação Ruby](https://ruby-doc.org/core-3.0.0/Float.html#method-i-round) para o método `round` de Floats.
- [Padrão IEEE para Aritmética de Ponto Flutuante (IEEE 754)](https://ieeexplore.ieee.org/document/4610935).
- [Entendendo a Precisão de Ponto Flutuante](https://floating-point-gui.de/), para um insight mais profundo sobre como os computadores lidam com números decimais.
