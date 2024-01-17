---
title:                "Lendo argumentos da linha de comando"
html_title:           "Ruby: Lendo argumentos da linha de comando"
simple_title:         "Lendo argumentos da linha de comando"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## O que & Porquê?

Ler os argumentos da linha de comando é uma habilidade importante para programadores em Ruby. Isso permite que o código receba informações inseridas pelo usuário na linha de comando ao executar o programa. Isso torna o programa mais interativo e permite que as ações do programa sejam controladas pelo usuário.

## Como fazer:

**Exemplo 1:**

```
Ruby nome_do_programa.rb argumento1 argumento2
```

**Exemplo 2:**

```
Ruby soma.rb 2 3
```

**Saída:**

```
5
```

No primeiro exemplo, o programa `nome_do_programa.rb` é executado com dois argumentos - `argumento1` e `argumento2`. No segundo exemplo, o programa `soma.rb` é executado com os números 2 e 3 como argumentos, e 5 é o resultado da soma desses dois números.

É importante notar que os argumentos na linha de comando são sempre tratados como strings em Ruby, então é necessário converter para o tipo desejado no código, se necessário.

## Mergulho Profundo:

**Contexto histórico:**

O conceito de ler argumentos da linha de comando é comum em linguagens de programação e sistemas operacionais desde as primeiras versões do Unix, lançadas na década de 1970. Com o uso cada vez mais frequente de interfaces de linha de comando, essa habilidade se tornou essencial para os programadores.

**Alternativas:**

Nem sempre é necessário ler argumentos da linha de comando em Ruby. Dependendo da finalidade do programa, pode ser mais eficiente ou adequado usar outras formas de interação com o usuário, como leitura de entrada do teclado durante a execução do programa.

**Detalhes de implementação:**

Em Ruby, a leitura de argumentos da linha de comando é feita usando a variável especial `$ARGV`, que é um array que contém todos os argumentos fornecidos pelo usuário. Além disso, a classe `OptionParser` da biblioteca padrão `optparse` fornece uma maneira mais abrangente e flexível de processar argumentos da linha de comando em Ruby.

## Veja também:

- [Documentação do Ruby sobre o objeto `$ARGV`](https://ruby-doc.org/core-3.0.0/globals_rdoc.html#label-Command-Line+Arguments)
- [Documentação do Ruby sobre a classe `OptionParser`](https://ruby-doc.org/stdlib-3.0.0/libdoc/optparse/rdoc/OptionParser.html)