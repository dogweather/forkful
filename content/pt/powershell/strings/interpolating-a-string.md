---
aliases:
- /pt/powershell/interpolating-a-string/
date: 2024-01-20 17:51:37.234550-07:00
description: "Interpola\xE7\xE3o de string permite injetar valores em uma cadeia de\
  \ caracteres, tornando seu c\xF3digo mais limpo e leg\xEDvel. Programadores fazem\
  \ isso para\u2026"
lastmod: 2024-02-18 23:08:58.353323
model: gpt-4-1106-preview
summary: "Interpola\xE7\xE3o de string permite injetar valores em uma cadeia de caracteres,\
  \ tornando seu c\xF3digo mais limpo e leg\xEDvel. Programadores fazem isso para\u2026"
title: Interpolando uma string
---

{{< edit_this_page >}}

## O Que é e Por Que?
Interpolação de string permite injetar valores em uma cadeia de caracteres, tornando seu código mais limpo e legível. Programadores fazem isso para construir strings dinamicamente e evitar a concatenação manual de dados e texto.

## Como Fazer:
```PowerShell
# Definindo uma variável
$nome = 'Mundo'

# Interpolando a variável dentro de uma string usando a sintaxe do Powershell
$saudacao = "Olá, $nome!"

# Exibindo o resultado
$saudacao
```
Saída esperada:
```
Olá, Mundo!
```

Mais um exemplo, com expressões:
```PowerShell
$numero = 15
$mensagem = "O dobro de $numero é $(2 * $numero)."
$mensagem
```
Saída esperada:
```
O dobro de 15 é 30.
```

## Mergulho Profundo
Antes de 2016, na versão 5 do PowerShell, a interpolação de strings era mais trabalhosa. A chegada do PowerShell 5 trouxe o uso de `$()` dentro de strings com aspas duplas para facilitar essa tarefa. Essa funcionalidade permite não somente incluir variáveis, mas também executar operações inline.

Como alternativa à interpolação, pode-se usar o operador `-f`, o que é útil quando se tem um template fixo de string e os valores são inseridos sequencialmente.

```PowerShell
$template = 'Olá, {0}! Hoje é {1}.'
$template -f 'Mundo', (Get-Date -Format 'dddd')
```

É importante lembrar que interpolação só acontece em strings delimitadas por aspas duplas (`"`). Aspas simples (`'`) são usadas para strings literais e não interpretam as variáveis dentro delas.

## Veja Também
- [Sobre_Quoting_Rules](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_quoting_rules) — Documentação oficial sobre regras de aspas no PowerShell.
- [Sobre_Operators](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_operators) — Saiba mais sobre operadores, incluindo o operador `-f` para formatação de strings.
- [Interpolação de String em C#](https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/tokens/interpolated) — Para entender como a interpolação de strings funciona em outras linguagens da família .NET.
