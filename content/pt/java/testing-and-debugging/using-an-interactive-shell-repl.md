---
date: 2024-01-26 04:15:21.513685-07:00
description: "Como fazer: Iniciar um REPL em Java \xE9 simples com a ferramenta `jshell`\
  \ introduzida no Java 9. Veja como colocar as m\xE3os nele e iniciar uma sess\xE3\
  o b\xE1sica."
lastmod: '2024-03-13T22:44:46.457691-06:00'
model: gpt-4-0125-preview
summary: "Iniciar um REPL em Java \xE9 simples com a ferramenta `jshell` introduzida\
  \ no Java 9."
title: Usando um shell interativo (REPL)
weight: 34
---

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
