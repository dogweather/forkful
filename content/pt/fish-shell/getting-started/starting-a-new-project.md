---
date: 2024-01-20 18:03:38.595870-07:00
description: "Come\xE7ar um novo projeto \xE9 como abrir um livro em branco, onde\
  \ voc\xEA pode escrever seus c\xF3digos do jeito que bem entender. Programadores\
  \ fazem isso para\u2026"
lastmod: 2024-02-19 22:05:06.074023
model: gpt-4-1106-preview
summary: "Come\xE7ar um novo projeto \xE9 como abrir um livro em branco, onde voc\xEA\
  \ pode escrever seus c\xF3digos do jeito que bem entender. Programadores fazem isso\
  \ para\u2026"
title: Iniciando um novo projeto
---

{{< edit_this_page >}}

## O Que & Porquê?
Começar um novo projeto é como abrir um livro em branco, onde você pode escrever seus códigos do jeito que bem entender. Programadores fazem isso para transformar ideias em realidade, solucionar problemas ou apenas por diversão.

## Como Fazer:
Vamos criar uma estrutura de diretórios para um novo projeto em Fish Shell. Rápido, fácil e direto ao ponto.

```Fish Shell
# Criar diretório do projeto e entrar nele
mkdir meu_projeto && cd meu_projeto

# Iniciar um repositório Git
git init

# Criar diretórios e arquivos básicos
mkdir src tests
touch src/main.fish tests/test_main.fish
echo "#!/usr/bin/env fish" > src/main.fish

# Tornar o script executável
chmod +x src/main.fish

# Configurar um ambiente virtual, se necessário
fisher install virtualfish
vf new meu_projeto_env
```

E aí está, seu projeto está pronto para você começar a codar.

## Mergulho Profundo:
Historicamente, o Fish Shell surgiu para oferecer um interpretador de comandos mais user-friendly, com funcionalidades como a auto-sugestão e a coloração de sintaxe. Em comparação com o Bash, o Fish tem como filosofia ser mais consistente e fácil de usar.

Como alternativa ao Fish, você pode usar o Bash, Zsh ou Powershell, mas cada um deles tem seu próprio conjunto de características e sintaxes.

Quando você inicia um novo projeto no Fish, é importante lembrar algumas diferenças-chave em relação a outros shells. Por exemplo, a atribuição de variáveis no Fish não usa o sinal de "$" para atribuir valores, e as funções são definidas com a palavra-chave `function`.

O uso de um sistema de controle de versão, como o Git, é crucial para gerenciar as mudanças no código ao longo do tempo, permitindo rollback e colaboração eficiente.

A utilização de ambientes virtuais, por meio de plugins como o `virtualfish`, ajuda a evitar conflitos entre dependências de diferentes projetos.

## Veja Também:
- Documentação oficial do Fish Shell [link](https://fishshell.com/docs/current/index.html)
- Tutorial de Git para iniciantes [link](https://git-scm.com/book/pt-br/v2)
- Site do VirtualFish [link](https://github.com/justinmayer/virtualfish)
