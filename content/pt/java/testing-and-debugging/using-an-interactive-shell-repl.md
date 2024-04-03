---
date: 2024-01-26 04:15:21.513685-07:00
description: "Um REPL (Read-Eval-Print Loop, ou La\xE7o de Leitura-Avalia\xE7\xE3\
  o-Impress\xE3o) \xE9 um shell interativo que processa entradas individuais do usu\xE1\
  rio, executa c\xF3digo\u2026"
lastmod: '2024-03-13T22:44:46.457691-06:00'
model: gpt-4-0125-preview
summary: "Um REPL (Read-Eval-Print Loop, ou La\xE7o de Leitura-Avalia\xE7\xE3o-Impress\xE3\
  o) \xE9 um shell interativo que processa entradas individuais do usu\xE1rio, executa\
  \ c\xF3digo e retorna o resultado."
title: Usando um shell interativo (REPL)
weight: 34
---

## O Que & Por Quê?
Um REPL (Read-Eval-Print Loop, ou Laço de Leitura-Avaliação-Impressão) é um shell interativo que processa entradas individuais do usuário, executa código e retorna o resultado. Os programadores utilizam-no para experimentos rápidos, depuração ou aprendizado, pois permite um feedback imediato e iteração.

## Como fazer:
Iniciar um REPL em Java é simples com a ferramenta `jshell` introduzida no Java 9. Veja como colocar as mãos nele e iniciar uma sessão básica:

```Java
jshell> int sum(int a, int b) {
   ...> return a + b;
   ...> }
|  método criado sum(int,int)

jshell> sum(5, 7)
$1 ==> 12
```

Saia a qualquer momento com `/exit`.

```Java
jshell> /exit
|  Adeus
```

## Aprofundando
Antes do `jshell`, os programadores Java não tinham um REPL oficial, diferente dos desenvolvedores Python ou Ruby. Eles usavam IDEs ou escreviam programas completos mesmo para tarefas triviais. O `jshell` foi um divisor de águas a partir do Java 9, preenchendo essa lacuna.

As alternativas incluem compiladores online ou plugins de IDE, mas eles não correspondem à imediatez do `jshell`. Quanto aos internos, o `jshell` usa a API do Compilador Java para executar fragmentos de código, o que é bastante interessante. É mais do que um playground — pode importar bibliotecas, definir classes e mais. Isso o torna uma ferramenta robusta para prototipagem.

## Veja também
- [Guia do Usuário do JShell](https://docs.oracle.com/javase/9/jshell/introduction-jshell.htm)
- [Referência de Ferramentas da Plataforma Java, Edição Padrão](https://docs.oracle.com/javase/9/tools/tools-and-command-reference.htm#JSWOR719)
- [API do Compilador Java](https://docs.oracle.com/javase/9/docs/api/javax/tools/JavaCompiler.html)
