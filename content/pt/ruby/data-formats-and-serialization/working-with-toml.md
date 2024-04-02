---
date: 2024-01-26 04:26:08.068017-07:00
description: "TOML \xE9 um formato de arquivo de configura\xE7\xE3o que \xE9 f\xE1\
  cil de ler devido \xE0 sua clareza sem\xE2ntica. Programadores usam TOML para gerenciar\
  \ configura\xE7\xF5es de\u2026"
lastmod: '2024-03-13T22:44:47.118676-06:00'
model: gpt-4-0125-preview
summary: "TOML \xE9 um formato de arquivo de configura\xE7\xE3o que \xE9 f\xE1cil\
  \ de ler devido \xE0 sua clareza sem\xE2ntica. Programadores usam TOML para gerenciar\
  \ configura\xE7\xF5es de\u2026"
title: Trabalhando com TOML
weight: 39
---

## O Que & Por Que?

TOML é um formato de arquivo de configuração que é fácil de ler devido à sua clareza semântica. Programadores usam TOML para gerenciar configurações de aplicativos e serialização de dados sem a complexidade do XML ou as peculiaridades do YAML.

## Como Fazer:

Primeiro, instale a gem `toml-rb`. É uma escolha popular para fazer o parsing de TOML em Ruby.

```Ruby
gem install toml-rb
```

Em seguida, lendo um arquivo TOML:

```Ruby
require 'toml-rb'

toml_content = File.read('config.toml')
config = TomlRB.parse(toml_content)
puts config['title']
```

Um exemplo de saída pode ser:

```
Meu Aplicativo Incrível
```

Escrevendo em um arquivo TOML:

```Ruby
require 'toml-rb'

config = {
  'title' => 'Meu Aplicativo Incrível',
  'owner' => {
    'name' => 'John Doe',
    'dob' => Date.new(1979, 5, 27)
  }
}

toml_string = TomlRB.dump(config)
File.write('config.toml', toml_string)
```

Verifique `config.toml` e você verá suas configurações, armazenadas de maneira limpa.

## Mergulho Profundo

TOML, que significa Tom's Obvious, Minimal Language (Linguagem Mínima Óbvia do Tom), foi criado por Tom Preston-Werner, co-fundador do GitHub, por volta de 2013. Seu principal objetivo é ser um formato direto que seja fácil de converter em estruturas de dados. Enquanto o JSON é ótimo para APIs, e o YAML é flexível, o nicho do TOML é sua ênfase em ser amigável para humanos. Ao contrário do YAML, que pode ser complicado com a indentação, o TOML visa uma estrutura mais parecida com a INI, que muitos consideram mais simples e menos propensa a erros.

Alternativas como JSON, YAML ou XML, cada um tem seus próprios pontos fortes, mas o TOML se destaca em cenários onde uma configuração deve ser facilmente mantida por humanos e programas de forma igual. Não é apenas mais simples, mas impõe uma formatação estrita e legível.

Do lado técnico, para fazer o parsing de conteúdo TOML com Ruby, utilizamos gems como `toml-rb`. Esta gem aproveita a natureza dinâmica do Ruby, convertendo dados TOML em hashes do Ruby nativo, arrays e outras estruturas de dados básicas. Essa conversão significa que os desenvolvedores podem trabalhar com dados TOML usando semântica e métodos familiares do Ruby.

## Veja Também

- Projeto e especificação de TOML: https://toml.io/pt/
- A gem `toml-rb`: https://github.com/emancu/toml-rb
- Comparando TOML, YAML e JSON: https://blog.theodo.com/2021/08/compare-yml-toml-json/
