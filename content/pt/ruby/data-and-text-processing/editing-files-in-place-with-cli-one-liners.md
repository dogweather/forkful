---
date: 2024-01-27 16:20:40.698457-07:00
description: "Editar arquivos in-place com one-liners de CLI (Interface de Linha de\
  \ Comando) em Ruby permite modificar arquivos diretamente do seu terminal, sem a\u2026"
lastmod: '2024-03-13T22:44:47.091417-06:00'
model: gpt-4-0125-preview
summary: "Editar arquivos in-place com one-liners de CLI (Interface de Linha de Comando)\
  \ em Ruby permite modificar arquivos diretamente do seu terminal, sem a\u2026"
title: Editando arquivos in loco com linhas de comando
weight: 32
---

## O Que & Por Quê?

Editar arquivos in-place com one-liners de CLI (Interface de Linha de Comando) em Ruby permite modificar arquivos diretamente do seu terminal, sem a necessidade de abri-los em um editor, fazer alterações e salvá-los de volta. Esta técnica é incrivelmente útil para modificações rápidas, atualizações em lote ou automação de tarefas repetitivas, economizando tempo e esforço.

## Como fazer:

Ruby oferece uma maneira direta de editar arquivos in-place diretamente da linha de comando. Usando o interruptor `-i` do Ruby, você pode dizer ao Ruby para operar diretamente nos arquivos fornecidos. Vamos brincar com alguns exemplos para ver como isso funciona na vida real. Imagine que você tem um arquivo `greetings.txt` com o seguinte conteúdo:

```
Hello, world!
Hello, Ruby!
Hello, programming!
```

E você quer substituir a palavra "Hello" por "Hi". Veja como você pode fazer isso:

```Ruby
ruby -i -pe "gsub(/Hello/, 'Hi')" greetings.txt
```

Após executar este comando, `greetings.txt` será atualizado para:

```
Hi, world!
Hi, Ruby!
Hi, programming!
```

Se você está preocupado em potencialmente bagunçar os dados, o Ruby te protege. Fornecendo uma extensão ao interruptor `-i`, o Ruby cria um backup antes de executar as mudanças. Por exemplo:

```Ruby
ruby -i.bak -pe "gsub(/Hello/, 'Bye')" greetings.txt
```

Agora, junto com o seu `greetings.txt` editado, você encontrará um `greetings.txt.bak` no mesmo diretório, contendo o conteúdo original.

## Aprofundamento

A mágica da edição de arquivos in-place em Ruby vem de sua combinação de capacidades de processamento de texto ao estilo Perl e a elegância sintática própria do Ruby. Historicamente, Perl era a linguagem de escolha para scripts de one-liner rápidos, especialmente para manipulação de texto. Ruby adotou este paradigma, permitindo capacidades poderosas de script de linha de comando.

Alternativas para edição in-place existem em outras linguagens, como o próprio Perl e sed, um editor de fluxo em sistemas Unix. Cada uma tem suas forças — Perl é conhecido por sua proeza no processamento de texto enquanto sed é incomparável em sua simplicidade para tarefas de edição de fluxo. No entanto, Ruby oferece um equilíbrio, proporcionando manipulação robusta de texto com uma sintaxe mais legível e amigável ao usuário, especialmente para aqueles já familiarizados com Ruby.

Na frente de implementação, a edição in-place do Ruby funciona renomeando o arquivo original, criando um novo com o nome de arquivo original e, em seguida, escrevendo as mudanças neste novo arquivo à medida que lê do original renomeado. Esta abordagem garante a atomicidade da operação; ou todo o arquivo é processado com sucesso ou nenhuma mudança é feita, protegendo a integridade dos seus dados durante o processo de edição. Este mecanismo, combinado com o tratamento de exceções do Ruby, também oferece resiliência contra interrupções, como quedas de energia ou encerramento de processos, garantindo que pelo menos o backup permaneça intacto.

Em resumo, a edição de arquivos in-place do Ruby é um testemunho de sua utilidade como uma linguagem de script, oferecendo uma mistura de poder, simplicidade e elegância para tarefas de manipulação de texto diretamente da linha de comando.
