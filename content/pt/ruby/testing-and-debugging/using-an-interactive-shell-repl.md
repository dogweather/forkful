---
date: 2024-01-26 04:17:26.260591-07:00
description: "Como Fazer: O REPL do Ruby \xE9 chamado de IRB (Interactive Ruby). Entre\
  \ e experimente o Ruby diretamente do seu terminal."
lastmod: '2024-03-13T22:44:47.097012-06:00'
model: gpt-4-0125-preview
summary: "O REPL do Ruby \xE9 chamado de IRB (Interactive Ruby)."
title: Usando um shell interativo (REPL)
weight: 34
---

## Como Fazer:
O REPL do Ruby é chamado de IRB (Interactive Ruby). Entre e experimente o Ruby diretamente do seu terminal:

```Ruby
irb
2.7.0 :001 > puts "Olá, mundo Ruby!"
Olá, mundo Ruby!
 => nil
2.7.0 :002 > 5.times { print "Ruby! " }
Ruby! Ruby! Ruby! Ruby! Ruby!  => 5
```

## Mergulho Profundo
Introduzido no Ruby 1.8, o IRB é um elemento essencial para os Rubyistas. Ele é inspirado nos shells interativos do Lisp e do Python, mesclando experimentação com feedback imediato. Alternativas como o Pry oferecem mais recursos como destaque de sintaxe e um ambiente de depuração mais robusto. O próprio IRB é simples, mas pode ser aprimorado com gems como 'irbtools' para estender a funcionalidade. A forma como o IRB lida com o ciclo de ler-avaliar-imprimir é lendo cada linha de entrada, avaliando-a como código Ruby e depois imprimindo o resultado, repetindo esse processo até a saída.

## Veja Também
- [IRB do Ruby](https://ruby-doc.org/stdlib-2.7.0/libdoc/irb/rdoc/IRB.html)
- [A gem irbtools](https://github.com/janlelis/irbtools)
