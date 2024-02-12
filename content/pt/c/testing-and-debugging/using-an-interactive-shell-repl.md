---
title:                "Usando um shell interativo (REPL)"
aliases: - /pt/c/using-an-interactive-shell-repl.md
date:                  2024-02-03T18:10:10.618692-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usando um shell interativo (REPL)"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/using-an-interactive-shell-repl.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O quê e Por quê?

Um shell interativo, também conhecido como Loop de Leitura-Avaliação-Impressão (REPL, na sigla em inglês), permite que programadores digitem expressões ou código e vejam os resultados imediatamente, melhorando os processos de aprendizado e depuração. Apesar de o C tradicionalmente não suportar ambientes REPL nativamente, ferramentas modernas preenchem essa lacuna, oferecendo exploração dinâmica de programas em C.

## Como fazer:

Para interagir com um REPL em C, você pode não encontrar um caminho tão direto quanto em linguagens como Python ou JavaScript. Entretanto, ferramentas como `Cling`, um interpretador de C/C++ baseado na tecnologia Clang e LLVM, tornam isso possível. Veja como começar:

1. **Instale o Cling**: Dependendo do seu sistema operacional, você pode encontrar o Cling em seu gerenciador de pacotes ou precisar construí-lo a partir do código fonte. Por exemplo, no Ubuntu, pode ser tão simples quanto `sudo apt-get install cling`.

2. **Iniciando o Cling**: Abra seu terminal e digite `cling` para iniciar o shell interativo.

```bash
$ cling
```

3. **Escrevendo Código**: Agora você pode digitar código C diretamente no shell e ver os resultados imediatamente. Aqui está um exemplo simples:

```c
[cling]$ #include <stdio.h>
[cling]$ printf("Olá, mundo REPL!\n");
Olá, mundo REPL!
```

4. **Exemplo com Variáveis e Operações**: Experimente com variáveis e veja feedback instantâneo.

```c
[cling]$ int a = 5;
[cling]$ int b = 3;
[cling]$ printf("%d + %d = %d\n", a, b, a+b);
5 + 3 = 8
```

5. **Incluindo Bibliotecas**: O Cling permite incluir bibliotecas de forma instantânea, possibilitando uma ampla gama de funcionalidades em C.

```c
[cling]$ #include <math.h>
[cling]$ printf("A raiz quadrada de %f é %f\n", 4.0, sqrt(4.0));
A raiz quadrada de 4.000000 é 2.000000
```

## Aprofundamento:

A origem dos ambientes REPL remonta ao Lisp nos anos 1960, projetados para suportar a avaliação interativa de código. No entanto, a natureza estática e compilada do C apresentou desafios para realizar a imediatez semelhante nos ajustes de execução de código. O desenvolvimento do Cling e outros interpretadores de C/C++ marca avanços significativos para integrar avaliação dinâmica em linguagens de tipagem estática.

Notavelmente, o uso de um interpretador como o Cling pode não espelhar perfeitamente o comportamento do código C compilado devido a diferenças em otimização e execução. Além disso, embora seja muito valioso para fins educacionais, prototipagem rápida e depuração, REPLs para C podem, às vezes, ser mais lentos e menos práticos para o desenvolvimento de código em nível de produção em comparação com ciclos tradicionais de compilação-execução-depuração.

Alternativas para programação interativa em C incluem a escrita de programas pequenos e autocontidos e o uso de IDEs robustos com ferramentas de depuração integradas, que podem oferecer mais controle e visão sobre a execução, embora com menos imediatismo. Apesar dessas alternativas, o advento dos ambientes REPL em C representa uma expansão emocionante da versatilidade da linguagem, abraçando as demandas da era moderna por flexibilidade e velocidade nos ciclos de desenvolvimento.
