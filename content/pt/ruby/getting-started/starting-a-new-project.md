---
date: 2024-01-20 18:04:17.118975-07:00
description: "Iniciar um novo projeto \xE9 como colocar a primeira pedra na constru\xE7\
  \xE3o de uma ideia. Programadores fazem isso para transformar solu\xE7\xF5es imagin\xE1\
  rias em\u2026"
lastmod: 2024-02-19 22:05:06.168484
model: gpt-4-1106-preview
summary: "Iniciar um novo projeto \xE9 como colocar a primeira pedra na constru\xE7\
  \xE3o de uma ideia. Programadores fazem isso para transformar solu\xE7\xF5es imagin\xE1\
  rias em\u2026"
title: Iniciando um novo projeto
---

{{< edit_this_page >}}

## O Que & Por Que?
Iniciar um novo projeto é como colocar a primeira pedra na construção de uma ideia. Programadores fazem isso para transformar soluções imaginárias em software que realmente funciona, resolvendo problemas reais ou satisfazendo necessidades.

## Como Fazer:
Vamos começar um projetinho em Ruby, mantendo as coisas simples. Primeiro, abra seu terminal e crie uma nova pasta para o projeto:

```bash
mkdir meu_projeto_ruby
cd meu_projeto_ruby
```

Agora, vamos iniciar um arquivo `main.rb` e escrever um clássico "Hello World" em Ruby:

```ruby
# main.rb
puts 'Olá, Mundo!'
```

Para rodar seu programa, use o seguinte comando no terminal:

```bash
ruby main.rb
```

E você verá o seguinte resultado:

```
Olá, Mundo!
```

Por fim, se quiser levar seu projeto a sério, considere usar `bundler` para gerenciar dependências e versões:

```bash
gem install bundler
bundler init
```

Isso criará um arquivo `Gemfile` no seu projeto. Adicione suas dependências nele e rode `bundle install`.

## Mergulho Profundo
Começar um novo projeto em Ruby hoje em dia é bem diferente de anos atrás. Naquela época, instalações e configurações de ambiente podiam demorar muito mais. Agora, com o `bundler`, gerenciar gemas específicas do projeto é quase um passeio no parque.

Além disso, versões antigas do Ruby não tinham tantas ferramentas e bibliotecas úteis disponíveis. Hoje, com `rails`, `sinatra`, ou até mesmo `dry-rb` para os que preferem abordagens minimalistas, Ruby oferece um ecossistema rico para dar vida a qualquer tipo de aplicação.

Implementar um novo projeto em Ruby também é sobre entender a cultura Ruby - as chamadas "The Ruby Way" - que favorece a produtividade e felicidade do programador com uma linguagem expressiva e intuitiva.

Há outras abordagens que você pode adotar no início de um projeto, como a TDD (Test-Driven Development), onde você escreverá testes antes do próprio código, garantindo que cada parte do seu sistema funciona como esperado desde o começo.

## Veja Também
- A [documentação oficial do Ruby](https://www.ruby-lang.org/pt/documentation/) é sempre o melhor ponto de partida para aprender e ficar por dentro das novidades do Ruby.
- O [RubyGems](https://rubygems.org/) é o lugar para encontrar e publicar gemas Ruby, indispensável para qualquer projeto.
- O [GitHub](https://guides.github.com/) oferece ótimos guias para versionamento e colaboração nos seus projetos Ruby.
- Para metodologias como TDD, o [RSpec](https://rspec.info/) é uma ferramenta poderosa que te auxilia a escrever testes melhores em Ruby.
