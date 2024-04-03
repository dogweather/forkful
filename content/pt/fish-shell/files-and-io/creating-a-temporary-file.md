---
date: 2024-01-20 17:40:00.934827-07:00
description: "How to: No Fish Shell, voc\xEA pode criar facilmente um arquivo tempor\xE1\
  rio usando a fun\xE7\xE3o `mktemp`."
lastmod: '2024-03-13T22:44:47.024976-06:00'
model: gpt-4-1106-preview
summary: "No Fish Shell, voc\xEA pode criar facilmente um arquivo tempor\xE1rio usando\
  \ a fun\xE7\xE3o `mktemp`."
title: "Criando um arquivo tempor\xE1rio"
weight: 21
---

## How to:
No Fish Shell, você pode criar facilmente um arquivo temporário usando a função `mktemp`:

```Fish Shell
set -l tmp_file (mktemp)
echo 'Dados temporários aqui' > $tmp_file
cat $tmp_file
# Deve exibir: Dados temporários aqui
```

Depois de usar o arquivo, não esqueça de apagá-lo:

```Fish Shell
rm $tmp_file
# Confirmação de exclusão não é necessária
```

## Deep Dive
Antes do `mktemp` se tornar um padrão, criávamos arquivos temporários manualmente, correndo o risco de conflito de nomes e problemas de segurança. O uso do `mktemp` no UNIX é histórico e essencial, pois garante um arquivo único e seguro. No Fish Shell, a geração segue o mesmo princípio, inserindo-se de forma nativa no ecossistema UNIX-like.

Alternativas incluem gerenciar arquivos temporários dentro dos próprios scripts, mas isso aumenta a complexidade e o risco. Outros shells têm métodos similares, mas a simplicidade do Fish é difícil de superar.

Internamente, o `mktemp` cria um arquivo num diretório designado para temporários, como `/tmp` no Linux, com direitos que impedem outros usuários de lerem ou escreverem no seu arquivo. Isto é crucial para manter a integridade e confidencialidade dos seus dados de trabalho.

## See Also
- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [mktemp man page](https://linux.die.net/man/1/mktemp)
- [UNIX Filesystem Hierarchy](https://en.wikipedia.org/wiki/Filesystem_Hierarchy_Standard)
