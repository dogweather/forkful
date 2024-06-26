---
date: 2024-01-26 00:57:05.478681-07:00
description: "Como fazer: Ruby utiliza `begin`, `rescue`, `ensure` e `end` para tratar\
  \ erros. Voc\xEA envolve o c\xF3digo arriscado em `begin` e `end`. Se um erro ocorrer,\u2026"
lastmod: '2024-03-13T22:44:47.102756-06:00'
model: gpt-4-1106-preview
summary: Ruby utiliza `begin`, `rescue`, `ensure` e `end` para tratar erros.
title: Tratamento de erros
weight: 16
---

## Como fazer:
Ruby utiliza `begin`, `rescue`, `ensure` e `end` para tratar erros. Você envolve o código arriscado em `begin` e `end`. Se um erro ocorrer, `rescue` entra em ação.

```Ruby
begin
  # Código arriscado vai aqui.
  puts 10 / 0
rescue ZeroDivisionError => e
  puts "Ops! Você não pode fazer isso: #{e.message}"
ensure
  puts "Isso sempre executa, com erro ou não."
end
```

Saída de Exemplo:
```
Ops! Você não pode fazer isso: dividido por 0
Isso sempre executa, com erro ou não.
```

## Aprofundando
Historicamente, o tratamento de erros em linguagens de programação evoluiu significativamente, com as linguagens mais antigas muitas vezes tendo mecanismos rudimentares ou inexistentes. O tratamento de exceções do Ruby é inspirado em linguagens como Python e Smalltalk.

Alternativas ao `begin-rescue` no Ruby incluem o uso de `rescue` em definições de métodos ou a utilização de `throw` e `catch` para controle de fluxo não padrão, embora eles não sejam usados para tratamento de erros típico.

Um detalhe interessante: as exceções em Ruby são objetos (instâncias da classe `Exception` e suas descendentes), então você pode definir classes de erro personalizadas e fazer mais do que apenas registrar erros — você pode transportar um estado rico pelo programa para um tratamento de erros mais robusto.

## Veja Também
- A documentação do Ruby sobre exceções e tratamento de erros: [ruby-doc.org](https://ruby-doc.org/core-3.1.0/doc/syntax/exceptions_rdoc.html)
- Um guia detalhado sobre as melhores práticas de tratamento de erros em Ruby: [thoughtbot.com](https://thoughtbot.com/blog/rescue-standarderror-not-exception)
