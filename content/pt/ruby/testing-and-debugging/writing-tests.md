---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:58.996269-07:00
description: "Testar em Ruby \xE9 verificar se o seu c\xF3digo se comporta como esperado\
  \ sob v\xE1rias condi\xE7\xF5es. Os programadores escrevem testes para garantir\
  \ corre\xE7\xE3o,\u2026"
lastmod: '2024-03-11T00:14:20.843824-06:00'
model: gpt-4-0125-preview
summary: "Testar em Ruby \xE9 verificar se o seu c\xF3digo se comporta como esperado\
  \ sob v\xE1rias condi\xE7\xF5es. Os programadores escrevem testes para garantir\
  \ corre\xE7\xE3o,\u2026"
title: Escrevendo testes
---

{{< edit_this_page >}}

## O Que & Por Quê?
Testar em Ruby é verificar se o seu código se comporta como esperado sob várias condições. Os programadores escrevem testes para garantir correção, prevenir regressões e facilitar o refatoramento, com o objetivo de ter aplicações robustas e mantíveis.

## Como fazer:
Ruby vem com uma biblioteca embutida chamada `Test::Unit` para escrever testes unitários, encapsulando práticas de teste dentro de estruturas simples. No entanto, a comunidade Ruby frequentemente prefere bibliotecas de terceiros como RSpec e Minitest devido à sua expressividade e flexibilidade aprimoradas.

### Usando `Test::Unit`:
```ruby
require 'test/unit'

class CalculatorTest < Test::Unit::TestCase
  def test_addition
    result = 2 + 2
    assert_equal 4, result
  end
end
```
Execute seu arquivo de teste a partir do terminal, e você deve obter uma saída indicando sucesso ou falha dos testes:
```
Loaded suite test_calculator
Started
.
Finished in 0.001288 seconds.
1 tests, 1 assertions, 0 failures, 0 errors, 0 pendings, 0 omissions, 0 notifications
100% passed
```

### Usando RSpec:
RSpec é um framework popular de BDD (Desenvolvimento Guiado por Comportamento) para Ruby. Instale a gem com `gem install rspec`, depois inicialize-a no seu projeto com `rspec --init`.

```ruby
# calculator_spec.rb
require_relative '../calculator'

describe Calculator do
  it 'adiciona corretamente dois números' do
    expect(Calculator.add(2, 2)).to eq(4)
  end
end
```
Execute testes com o comando `rspec`. Exemplo de saída:
```
.

Finished in 0.002 seconds (files took 0.1 seconds to load)
1 example, 0 failures
```

### Usando Minitest:
Minitest oferece uma suíte completa de facilidades de teste que suportam TDD, BDD, mocks e benchmarking. Instale-a com `gem install minitest` e use da seguinte forma:

```ruby
# test_calculator.rb
require 'minitest/autorun'
require_relative '../calculator'

class CalculatorTest < Minitest::Test
  def test_addition
    assert_equal 4, Calculator.add(2, 2)
  end
end
```

Execute seu arquivo de teste direto ou através da tarefa `rake` configurada para minitest. Exemplo de saída:
```
Run options: --seed 33407

# Running:

.

Finished in 0.001027s, 974.5922 runs/s, 974.5922 assertions/s.
1 runs, 1 assertions, 0 failures, 0 errors, 0 skips
```

Implementando testes nos seus projetos Ruby usando essas bibliotecas, você adere às melhores práticas, levando a bases de código mais confiáveis e mantíveis.
