---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:09.001352-07:00
description: "Como fazer: No Fish Shell, voc\xEA n\xE3o possui comandos integrados\
  \ especificamente projetados para a an\xE1lise de datas de strings. Em vez disso,\
  \ voc\xEA depende\u2026"
lastmod: '2024-03-13T22:44:47.015306-06:00'
model: gpt-4-0125-preview
summary: "No Fish Shell, voc\xEA n\xE3o possui comandos integrados especificamente\
  \ projetados para a an\xE1lise de datas de strings."
title: Analisando uma data a partir de uma string
weight: 30
---

## Como fazer:
No Fish Shell, você não possui comandos integrados especificamente projetados para a análise de datas de strings. Em vez disso, você depende de utilitários externos como `date` (disponível no Linux e macOS) ou aproveita ferramentas de terceiros populares como o `GNU date` para uma análise mais complexa. Aqui está como abordá-lo:

**Usando `date` com Fish:**

Para analisar uma string de data no formato "AAAA-MM-DD", você pode usar o comando `date` com a opção `-d` (ou `--date` para o GNU date) seguido pela string. A opção `+` é usada para formatar a saída.

```fish
set date_str "2023-04-01"
date -d $date_str +"%A, %d %B %Y"
# Saída: Saturday, 01 April 2023
```

Para macOS (que requer um formato diferente para as flags `-j` e `-f`):

```fish
set date_str "2023-04-01"
date -j -f "%Y-%m-%d" $date_str +"%A, %d %B %Y"
# Saída: Saturday, 01 April 2023
```

**Usando `GNU date` para análises complexas:** 

O `GNU date` é mais flexível com formatos de string. Ele pode detectar automaticamente muitos formatos comuns de string de data sem especificar explicitamente o formato de entrada:

```fish
set complex_date_str "April 1, 2023 14:00"
date -d "$complex_date_str" '+%Y-%m-%d %H:%M:%S'
# Saída: 2023-04-01 14:00:00
```

No entanto, ao trabalhar com strings de data que podem não ser reconhecidas automaticamente ou quando um controle preciso sobre o formato de entrada é necessário, especificar o formato de entrada com `GNU date` não é diretamente suportado. Nesses casos, considere pré-processar a string ou usar outra ferramenta projetada para rotinas de análise de data mais complexas.
