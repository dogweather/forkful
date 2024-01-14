---
title:    "Ruby: Iniciando um novo projeto"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Por que iniciar um novo projeto em Ruby?

Iniciar um novo projeto em Ruby pode ser uma ótima escolha para aqueles que estão procurando uma linguagem de programação flexível e fácil de aprender. Além disso, a comunidade de desenvolvedores Ruby é bastante ativa e há uma abundância de recursos disponíveis para ajudá-lo em sua jornada de aprendizado.

# Como começar

Para começar um novo projeto em Ruby, você precisará primeiro ter o Ruby instalado em seu computador. Em seguida, siga as etapas abaixo para configurar seu ambiente de desenvolvimento:

```ruby
# Instalar o Ruby através de um gerenciador de pacotes ou diretamente do site oficial (https://www.ruby-lang.org/pt/)

# Verificar se a instalação foi bem-sucedida com o comando abaixo:
ruby -v

# Instalar o bundler, uma ferramenta útil para gerenciar as dependências de seu projeto:
gem install bundler

# Criar um diretório para o seu projeto:
mkdir meu_projeto

# Acessar o diretório criado:
cd meu_projeto

# Inicializar um arquivo Gemfile:
bundle init
```

Com o ambiente devidamente configurado, agora é hora de codificar seu projeto em Ruby!

# Mergulhando mais fundo

Ao iniciar um novo projeto em Ruby, é importante entender a estrutura de um projeto típico em Ruby. A maioria dos projetos seguirá uma estrutura semelhante à abaixo:

- Pasta `lib`: essa é a pasta onde você colocará todo o código Ruby que você escrever para seu projeto.
- Pasta `spec`: aqui é onde você escreverá seus testes para garantir que seu código está funcionando corretamente.
- Arquivo `Gemfile`: este arquivo irá listar todas as dependências de seu projeto.
- Arquivo `Rakefile`: este arquivo contém ações que podem ser executadas em seu projeto, como executar testes ou criar documentação.

Você também pode querer considerar o uso de algumas gems populares em seu projeto, como o framework de desenvolvimento web Ruby on Rails ou o framework de testes RSpec.

# Veja também

- [Instalando o Ruby](https://www.ruby-lang.org/pt/documentation/installation/)
- [Documentação do Ruby](https://ruby-doc.org/)
- [Ruby on Rails](https://rubyonrails.org/)
- [RSpec](https://rspec.info/)