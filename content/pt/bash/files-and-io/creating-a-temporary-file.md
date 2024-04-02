---
date: 2024-01-20 17:39:33.123481-07:00
description: "Criar um arquivo tempor\xE1rio \xE9 o processo de fazer um arquivo que\
  \ s\xF3 existe durante a execu\xE7\xE3o do programa ou at\xE9 ser explicitamente\
  \ removido.\u2026"
lastmod: '2024-03-13T22:44:46.772471-06:00'
model: gpt-4-1106-preview
summary: "Criar um arquivo tempor\xE1rio \xE9 o processo de fazer um arquivo que s\xF3\
  \ existe durante a execu\xE7\xE3o do programa ou at\xE9 ser explicitamente removido.\u2026"
title: "Criando um arquivo tempor\xE1rio"
weight: 21
---

## O Que & Por Que?
Criar um arquivo temporário é o processo de fazer um arquivo que só existe durante a execução do programa ou até ser explicitamente removido. Programadores fazem isso para armazenar dados temporários sem correr o risco de conflito com outros arquivos ou processos e para garantir que os resíduos não se acumulem no sistema.

## Como Fazer:
```Bash
# Criar um arquivo temporário com mktemp
temp_file=$(mktemp)

# Usar o arquivo temporário
echo "Dados temporários" > "$temp_file"

# Verificar conteúdo do arquivo
cat "$temp_file"  

# Limpar: Remover o arquivo temporário quando terminar
rm "$temp_file"  
```
```Bash
# Saída esperada após o cat
Dados temporários
```

## Aprofundando
Originalmente, arquivos temporários eram criados manualmente, com o risco de colisão de nomes e falhas de segurança. Comandos como `mktemp`, introduzidos mais tarde, geram arquivos com nomes únicos e seguros em `/tmp`. Alternativas incluem a criação de arquivos temporários na própria pasta de trabalho do script, mas a prática não é recomendada devido ao risco maior de conflitos e problemas de limpeza. A implementação do `mktemp` no Bash é uma chamada direta ao comando `mktemp` do sistema, que lida com a criação do arquivo real e retorna o caminho que o script pode então utilizar.

## Veja Também
- [GNU Coreutils mktemp](https://www.gnu.org/software/coreutils/manual/html_node/mktemp-invocation.html)
- [Bash Guide for Beginners](https://tldp.org/LDP/Bash-Beginners-Guide/html/)
- [Advanced Bash-Scripting Guide](https://tldp.org/LDP/abs/html/)
