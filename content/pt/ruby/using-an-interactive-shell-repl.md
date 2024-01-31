---
title:                "Usando um shell interativo (REPL)"
date:                  2024-01-26T04:17:26.260591-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usando um shell interativo (REPL)"

category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## O Que & Por Que?
Um shell interativo, ou REPL (Read-Eval-Print Loop, Ciclo de Ler-Avaliar-Imprimir), permite testar código em tempo real. Programadores o utilizam para experimentar, depurar e aprender as nuances do Ruby sem criar scripts completos.

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
