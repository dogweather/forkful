---
title:                "Ruby: Escrita de testes"
programming_language: "Ruby"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/writing-tests.md"
---

{{< edit_this_page >}}

## Por que escrever testes em Ruby?

Escrever testes é uma prática fundamental para garantir que o seu código em Ruby funcione corretamente. Além disso, testes bem escritos ajudam a identificar erros e bugs no código, facilitando a manutenção do mesmo no longo prazo.

## Como escrever testes em Ruby

Para escrever testes em Ruby, é necessário usar uma ferramenta de teste como o RSpec. Primeiro, instale o RSpec na sua máquina usando o comando `gem install rspec`. Em seguida, crie um arquivo de teste com a extensão `.spec.rb` e adicione os seguintes códigos:

```Ruby
# arquivo de teste
require "rspec"

describe "Calculadora" do
  it "deve somar dois números corretamente" do
    resultado = 2 + 2
    expect(resultado).to eq(4)
  end 
end
```

Para rodar o teste, basta ir até o terminal e digitar `rspec nome_do_arquivo.spec.rb`. Você verá uma mensagem de sucesso caso o teste passe ou uma mensagem de erro caso o teste falhe.

## Profundidade nos testes em Ruby

Existem diversas maneiras de escrever testes em Ruby, é importante encontrar a que se adapta melhor ao tipo de aplicação que está sendo testada. Alguns conceitos importantes a se conhecer sobre testes em Ruby incluem doubles, stubs e mocks. Além disso, é sempre recomendado cobrir o máximo possível do código com testes, incluindo casos de borda e erros.

## Veja também

- [Documentação do RSpec](https://rspec.info/documentation/)
- [Tudo o que você precisa saber sobre testes em Ruby](https://rubygarage.org/blog/how-to-test-ruby)
- [10 bons motivos para escrever testes em Ruby](https://www.sitepoint.com/10-reasons-to-test-your-ruby-code/)