---
date: 2024-01-20 17:39:33.123481-07:00
description: "Como Fazer: Originalmente, arquivos tempor\xE1rios eram criados manualmente,\
  \ com o risco de colis\xE3o de nomes e falhas de seguran\xE7a. Comandos como `mktemp`,\u2026"
lastmod: '2024-04-05T21:53:47.118979-06:00'
model: gpt-4-1106-preview
summary: "Originalmente, arquivos tempor\xE1rios eram criados manualmente, com o risco\
  \ de colis\xE3o de nomes e falhas de seguran\xE7a."
title: "Criando um arquivo tempor\xE1rio"
weight: 21
---

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
