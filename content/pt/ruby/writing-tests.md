---
title:                "Escrevendo testes"
html_title:           "Ruby: Escrevendo testes"
simple_title:         "Escrevendo testes"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/writing-tests.md"
---

{{< edit_this_page >}}

## Por que escrever testes?

Escrever testes é uma prática comum e recomendada na programação, especialmente em Ruby. Testes garantem que o código escrito funcione de acordo com o esperado, além de facilitar a manutenção e refatoração do código.

## Como escrever testes em Ruby

Para escrever testes em Ruby, utilizamos a biblioteca padrão MiniTest ou a popular RSpec. Ambas fornecem uma sintaxe simples e eficiente para escrever testes. Vamos ver um exemplo utilizando o RSpec:

```Ruby
# Define a classe Calculator
class Calculator
  # Método de soma
  def sum(a, b)
    a + b
  end
end

# Teste utilizando o RSpec
RSpec.describe Calculator do
  describe "#sum" do
    it "returns the sum of two numbers" do
      calculator = Calculator.new
      expect(calculator.sum(2, 3)).to eq(5)
    end
  end
end
```

No exemplo acima, definimos uma classe `Calculator` com um método `sum` que retorna a soma de dois números. Em seguida, no teste utilizando o RSpec, utilizamos a sintaxe `expect` para verificar se a soma é realmente calculada corretamente. Ao executar este teste, ele deve passar sem problemas.

Este é apenas um exemplo simples, mas é importante notar que em testes reais, escrevemos casos para cobrir diferentes cenários e possíveis falhas no código.

## Aprofundando nos testes

Além dos testes unitários, que focam em testar pequenas unidades de código, existem outros tipos de testes que podemos escrever. Entre eles, podemos citar os testes de integração, que garantem que o código interage corretamente com outros componentes do sistema, e os testes de aceitação, que verificam se o software está atendendo aos requisitos do cliente.

Além disso, é importante lembrar que escrever testes não é apenas uma questão de escolha ou preferência, mas uma prática necessária para garantir a qualidade do código e a confiabilidade do software.

## Veja também

- [Documentação do MiniTest](https://ruby-doc.org/stdlib-2.7.1/libdoc/minitest/rdoc/MiniTest.html)
- [Documentação do RSpec](https://rspec.info/documentation/)