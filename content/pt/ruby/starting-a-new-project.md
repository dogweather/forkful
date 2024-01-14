---
title:                "Ruby: Iniciando um novo projeto"
programming_language: "Ruby"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Porque

Existem várias razões pelas quais alguém pode querer iniciar um novo projeto de programação em Ruby. Pode ser para aprimorar suas habilidades de codificação, aprender uma nova tecnologia ou até mesmo criar uma solução para um problema específico.

Independentemente da motivação, começar um novo projeto é uma ótima maneira de expandir seus conhecimentos e melhorar suas habilidades como programador.

## Como Fazer

Começar um novo projeto em Ruby é bastante simples. Primeiro, certifique-se de ter o Ruby instalado em sua máquina. Se ainda não tiver, você pode baixá-lo gratuitamente em [ruby-lang.org] (https://www.ruby-lang.org/pt/).

Depois de ter o Ruby instalado, você pode seguir estas etapas para configurar um novo projeto:

```Ruby
# 1) Crie um diretório para o seu projeto
mkdir meu_projeto

# 2) Navegue para o diretório
cd meu_projeto

# 3) Crie um arquivo Gemfile para gerenciar as dependências
touch Gemfile

# 4) Adicione a seguinte linha ao Gemfile para instalar a biblioteca padrão do Ruby (gems)
source 'https://rubygems.org'

# 5) Adicione as gems que você deseja usar em seu projeto (por exemplo, Rails)
gem 'rails'

# 6) Instale as gems
bundle install

# 7) Crie um arquivo "app.rb" para iniciar seu aplicativo e adicione o seguinte código
puts "Olá, mundo!"

# 8) Execute seu aplicativo
ruby app.rb
```

Se tudo correr bem, você deverá ver a saída "Olá, mundo!" no seu terminal.

## Deep Dive

Para aprofundar nosso conhecimento sobre iniciar um novo projeto em Ruby, vamos discutir algumas coisas importantes que podem ser úteis ao iniciar um projeto.

### Gerenciamento de Dependências

O Gemfile e o bundle install são duas ferramentas essenciais para gerenciar as dependências do seu projeto em Ruby. O Gemfile é um arquivo de configuração que lista as gems que seu projeto irá necessitar. O bundle install é o comando que instala todas as gems listadas no Gemfile.

Com a ajuda do bundle, você pode ter certeza de que todas as gems necessárias estão instaladas em sua máquina e atualizá-las facilmente se necessário.

### Estrutura de Diretórios Padrão

Ao iniciar um novo projeto em Ruby, é sempre uma boa prática seguir a estrutura padrão de diretórios. O Rails, por exemplo, segue a convenção de ter uma pasta "app" para o código da aplicação, uma pasta "config" para a configuração do projeto e uma pasta "lib" para suas bibliotecas personalizadas. Seguir uma estrutura de diretórios padrão pode tornar seu projeto mais organizado e fácil de gerenciar.

### Documentação

Documentar seu código é uma ótima maneira de ajudar outras pessoas a entenderem seu projeto e também pode ser útil para você mesmo em projetos futuros. O Ruby tem uma ferramenta chamada RDoc, que pode gerar documentação a partir do seu código Ruby. Você também pode adicionar comentários e documentação específicos ao seu código usando as tags RDoc.

## Veja Também

- [Ruby on Rails Guides] (https:rubyonrails.org.br/)
- [Documentação RDoc] (https://ruby-doc.org/stdlib-2.5.3/libdoc/rdoc/rdoc/rdoc_index.html)
- [Tutorial Ruby para Iniciantes] (https://www.ruby-lang.org/pt/documentation/quickstart/)