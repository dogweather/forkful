---
title:                "Escrevendo testes"
html_title:           "Arduino: Escrevendo testes"
simple_title:         "Escrevendo testes"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/writing-tests.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Escrever testes é criar cenários que verificam se peças do código funcionam como esperado. Programadores testam para garantir qualidade, prevenir bugs e facilitar manutenção.

## Como Fazer:

Para testar em Ruby, geralmente se usa a gem `RSpec`. Instale com `gem install rspec` e crie um arquivo de teste assim:

```Ruby
# spec/calculadora_spec.rb
require_relative '../calculadora'

describe Calculadora do
  it 'soma dois números' do
    expect(Calculadora.soma(5, 3)).to eq(8)
  end
end
```

No seu código principal:

```Ruby
# calculadora.rb
class Calculadora
  def self.soma(a, b)
    a + b
  end
end
```

Para rodar o teste, use: `rspec spec/calculadora_spec.rb`

A saída esperada é:

```
.

Finished in 0.00276 seconds (files took 0.15707 seconds to load)
1 example, 0 failures
```

## Aprofundamento

Testes em Ruby vêm desde a versão 1.8 com o `Test::Unit`, que depois evoluiu para o `MiniTest`. RSpec é popular pela sua linguagem mais natural e recursos poderosos. Ao implementar testes, use TDD (Test-Driven Development) para melhorar a estrutura do código e refatoração.

## Veja Também

- [Ruby Testing Guidelines](https://github.com/rubocop/ruby-style-guide#testing)
- [Test-Driven Development: By Example](http://www.oreilly.com/catalog/9780321146533) de Kent Beck
