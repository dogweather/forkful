---
title:                "Usando um shell interativo (REPL)"
date:                  2024-01-26T04:15:21.513685-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usando um shell interativo (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

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
