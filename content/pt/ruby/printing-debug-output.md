---
title:                "Exibindo saídas de depuração"
date:                  2024-01-20T17:53:36.912117-07:00
model:                 gpt-4-1106-preview
simple_title:         "Exibindo saídas de depuração"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

## O Que é & Porquê?
Imprimir saídas de debug é escrever no console aquilo que o seu código está fazendo, para que você possa entender melhor e corrigir o comportamento. Programadores fazem isso para verificar o fluxo do programa, valores de variáveis e encontrar bugs mais rapidamente.

## Como Fazer:
```Ruby
# Exemplo simples de imprimir uma variável.
variavel = "Olá, mundo!"
puts variavel # Saída: Olá, mundo!

# Uso de 'p' para ver a representação literal.
p variavel # Saída: "Olá, mundo!"

# Debugging com interpolação de string.
numero = 42
puts "O valor do número é #{numero}" # Saída: O valor do número é 42

# Usando o método 'inspect' para obter a forma de string de um objeto.
puts variavel.inspect # Saída: "Olá, mundo!"

# Imprimindo uma lista.
lista = [1, 'dois', :tres]
puts "Conteúdo da lista: #{lista.inspect}" # Saída: Conteúdo da lista: [1, "dois", :tres]
```

## Detalhes Profundos:
O debug é uma técnica antiga, quase tão velha quanto a programação em si. Antigamente, era comum utilizar luzes e switches físicos para entender o que um programa estava fazendo. Com a evolução das linguagens, começaram-se a usar instruções de impressão no código para acompanhar o fluxo de execução e entender o estado interno do programa. Alternativas modernas ao `puts` e `p` no Ruby incluem `logger` para aplicações mais complexas, que podem gravar informações de debug em arquivos ou outros destinos além do console.

Outros detalhes de implementação podem incluir a decisão de saídas para STDOUT (saída padrão) ou STDERR (saída de erro), útil para diferenciar tipos de saída quando se executa um programa. A estética e a praticidade também podem ser melhoradas com ferramentas como a gem 'awesome_print' que formatam a saída de forma mais legível e colorida.

## Veja Também:
- Documentação oficial do Ruby sobre métodos de saída: https://ruby-doc.org/core/IO.html
- Gem 'logger' para uma ferramenta de logging mais avançada: https://ruby-doc.org/stdlib/libdoc/logger/rdoc/Logger.html
- Gem 'awesome_print' para imprimir saídas bonitas e formatadas: https://github.com/awesome-print/awesome_print
