---
title:    "Ruby: Imprimindo saída de depuração"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Por que

Imprimir informações de depuração é uma técnica essencial para entender o comportamento do seu código. Pode ajudar a identificar erros, encontrar gargalos de desempenho e entender melhor como o seu programa está funcionando.

## Como Fazer

Aqui está um exemplo simples de como imprimir informações de depuração no seu código Ruby usando o comando `puts`:

```ruby
nome = "João"
idade = 27
puts "O meu nome é #{nome} e tenho #{idade} anos."
```

O resultado será: `O meu nome é João e tenho 27 anos.`

Você também pode usar o método `p` para imprimir objetos e variáveis, o que irá mostrar além do valor, também o seu tipo de dado. Por exemplo:

```ruby
array = [1, 2, 3]
p array
```

A saída será: `[1, 2, 3]` ao invés de apenas `1, 2, 3` se tivéssemos usado `puts`.

Além disso, você pode usar a gem `debug` para tornar a impressão de debug ainda mais fácil. Ela permite que você imprima os valores das variáveis automaticamente sem precisar usar comandos de impressão.

## Aprofundando

Imprimir informações de depuração é particularmente útil quando você está tentando solucionar bugs em seu código. Você pode colocar comandos de impressão em pontos estratégicos do seu código para entender como as variáveis estão sendo modificadas e como o fluxo do programa está acontecendo.

Outra técnica útil é o uso da flag `-d` ao executar um programa Ruby. Isso irá ativar o modo de debug e mostrará informações como a pilha de chamadas de métodos e o valor das variáveis em cada etapa.

Além disso, vale ressaltar que é importante remover quaisquer comandos de impressão desnecessários do seu código antes de lançar seu programa em produção. Eles podem impactar negativamente o desempenho e tornar seu código confuso.

## Veja também

- [Documentação oficial do Ruby sobre imprimir informações de depuração](https://docs.ruby-lang.org/en/latest/doc/debug.html)
- [Artigo do site Rails Guides sobre depuração em Ruby on Rails](https://guides.rubyonrails.org/debugging_rails_applications.html)
- [Gem debug no RubyGems.org](https://rubygems.org/gems/debug)