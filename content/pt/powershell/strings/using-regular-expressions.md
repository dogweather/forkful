---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:44.570055-07:00
description: "Como fazer: No PowerShell, voc\xEA pode usar os operadores `-match`,\
  \ `-replace` e `-split`, entre outros, para realizar a\xE7\xF5es com express\xF5\
  es regulares. Vamos\u2026"
lastmod: '2024-03-13T22:44:46.785403-06:00'
model: gpt-4-0125-preview
summary: "No PowerShell, voc\xEA pode usar os operadores `-match`, `-replace` e `-split`,\
  \ entre outros, para realizar a\xE7\xF5es com express\xF5es regulares."
title: "Usando express\xF5es regulares"
weight: 11
---

## Como fazer:
No PowerShell, você pode usar os operadores `-match`, `-replace` e `-split`, entre outros, para realizar ações com expressões regulares. Vamos explorar alguns exemplos:

### Usando `-match` para verificar se uma string corresponde a um padrão
Este operador retorna `$true` se o padrão for encontrado dentro da string, e `$false` caso contrário.

```powershell
"hello world" -match "\w+orld"
# Saída: True
```

### Extraindo correspondências
Você pode extrair o valor correspondido acessando a variável automática `$matches`.

```powershell
if ("I have 100 apples" -match "\d+") {
    "Número encontrado: " + $matches[0]
}
# Saída: Número encontrado: 100
```

### Usando `-replace` para substituições
O operador `-replace` substitui todas as ocorrências de um padrão por uma string de substituição especificada.

```powershell
"foo bar baz" -replace "ba[rz]", "qux"
# Saída: foo qux qux
```

### Dividindo strings com `-split`
Divida uma string em um array de substrings baseado em um padrão de regex.

```powershell
"The quick-brown_fox jumps" -split "[-_ ]"
# Saída: The quick brown fox jumps
```

### Correspondência de Padrões Avançada
O PowerShell também suporta operações de regex mais complexas por meio da classe `[regex]`, dando acesso a métodos como `Matches()`, `Replace()` e `Split()`.

```powershell
[regex]::Matches("June 24, August 9, Dec 12", "\b[A-Za-z]+\b").Value
# Saída: June August Dec

[regex]::Replace("100,000", "\B(?=(?:\d{3})+(?!\d))", ",")
# Saída: 100,000

[regex]::Split("one,two;three four", ",|;| ")
# Saída: one two three four
```

Esses exemplos mostram o poder e a versatilidade das expressões regulares no PowerShell para manipulação de dados e correspondência de padrões. Ao aproveitar regex, programadores podem realizar processamento de texto complexo de forma eficiente.
