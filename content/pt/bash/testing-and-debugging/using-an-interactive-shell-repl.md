---
title:                "Usando um shell interativo (REPL)"
date:                  2024-01-26T04:11:22.119219-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usando um shell interativo (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
REPL significa Loop de Ler-Avaliar-Imprimir, um ambiente de programação computacional simples e interativo. Programadores o utilizam para escrever e testar código rapidamente, experimentar com sintaxe e aprender conceitos de programação sem o peso de criar e executar aplicações inteiras.

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
