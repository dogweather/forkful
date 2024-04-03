---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:10:10.618692-07:00
description: "Um shell interativo, tamb\xE9m conhecido como Loop de Leitura-Avalia\xE7\
  \xE3o-Impress\xE3o (REPL, na sigla em ingl\xEAs), permite que programadores digitem\
  \ express\xF5es ou\u2026"
lastmod: '2024-03-13T22:44:47.051803-06:00'
model: gpt-4-0125-preview
summary: "Um shell interativo, tamb\xE9m conhecido como Loop de Leitura-Avalia\xE7\
  \xE3o-Impress\xE3o (REPL, na sigla em ingl\xEAs), permite que programadores digitem\
  \ express\xF5es ou c\xF3digo e vejam os resultados imediatamente, melhorando os\
  \ processos de aprendizado e depura\xE7\xE3o."
title: Usando um shell interativo (REPL)
weight: 34
---

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
