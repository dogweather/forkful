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

## O que & por que?

Escrever testes é uma prática muito importante para programadores. Isso envolve criar código que verifica se o código principal de um programa está funcionando corretamente. Isso pode economizar tempo e evitar a ocorrência de erros no código.

## Como fazer:

```Ruby 
# Exemplo de teste utilizando a biblioteca RSpec
describe "Calculadora" do 
  it "deve somar dois números" do
    resultado = soma(2, 5)
    expect(resultado).to eq(7)
  end
end
```

Saída do teste:

```
1 exemplo, 0 falhas
```

## Mergulho profundo:

Historicamente, a prática de escrever testes surgiu como uma forma de garantir a qualidade de código em projetos grandes e complexos. Além do RSpec, existem outras bibliotecas populares para testes em Ruby, como o MiniTest e o Capybara. Também é possível escrever testes utilizando a ferramenta builtin do Ruby, a miniunit. Implementar testes traz benefícios como identificar erros rapidamente, documentar o código e facilitar a identificação de falhas em atualizações futuras.

## Veja também:

- [RSpec documentação](https://rspec.info/documentation/)
- [Tutorial do MiniTest](https://semaphoreci.com/community/tutorials/getting-started-with-minitest)
- [Documentação do Capybara](https://github.com/teamcapybara/capybara)