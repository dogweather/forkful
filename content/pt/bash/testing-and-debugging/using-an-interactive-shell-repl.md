---
date: 2024-01-26 04:11:22.119219-07:00
description: "Como fazer: No Bash, seu terminal \xE9 essencialmente um REPL. Voc\xEA\
  \ digita um comando; ele l\xEA, avalia, imprime o resultado e volta ao ponto de\
  \ espera pelo\u2026"
lastmod: '2024-03-13T22:44:46.754327-06:00'
model: gpt-4-0125-preview
summary: "No Bash, seu terminal \xE9 essencialmente um REPL."
title: Usando um shell interativo (REPL)
weight: 34
---

## Como fazer:
No Bash, seu terminal é essencialmente um REPL. Você digita um comando; ele lê, avalia, imprime o resultado e volta ao ponto de espera pelo seu próximo comando. Aqui está um exemplo de como usar o Bash como um REPL:

```Bash
$ echo "Olá, Mundo!"
Olá, Mundo!
$ x=$((6 * 7))
$ echo $x
42
```

Sua entrada segue o prompt `$ `, com a saída impressa na próxima linha. Simples, certo?

## Mergulho Profundo
Bash, abreviação de Bourne Again SHell, é o shell padrão em muitos sistemas baseados em Unix. É uma atualização do original Bourne shell, construído no final dos anos 1970. Embora o Bash seja uma poderosa ferramenta de script, seu modo interativo permite que você execute comandos linha por linha.

Quando se considera alternativas, você tem o REPL do Python (simplesmente digite `python` no seu terminal), Node.js (com `node`) e IPython, um shell Python interativo aprimorado. Cada linguagem tende a ter sua própria implementação de REPL.

Por baixo dos panos, os REPLs são laços que analisam sua entrada (comandos ou código), executam-na e retornam o resultado para stdout (sua tela), muitas vezes usando diretamente o interpretador da linguagem. Esta imediatez do feedback é excelente para aprender e prototipar.

## Veja Também
- [Documentação Oficial do GNU Bash](https://gnu.org/software/bash/manual/bash.html)
- [Tutorial Interativo Learn Shell](https://www.learnshell.org/)
- [Site Oficial do IPython](https://ipython.org/)
- [REPL.it](https://replit.com/): Um REPL online multi-linguagem (Não apenas Bash!)
