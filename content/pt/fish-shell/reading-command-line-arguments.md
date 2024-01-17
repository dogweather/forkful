---
title:                "Lendo argumentos da linha de comando"
html_title:           "Fish Shell: Lendo argumentos da linha de comando"
simple_title:         "Lendo argumentos da linha de comando"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## O quê e por quê?

Ler argumentos da linha de comando é um processo comum para os programadores. Isso envolve acessar e ler os dados passados ​​para um programa a partir da linha de comando. Os programadores fazem isso para tornar seus programas mais interativos e para permitir que os usuários forneçam informações relevantes durante a execução do programa.

## Como fazer:

```
Fish Shell tem suporte integrado para ler argumentos da linha de comando. Você pode acessar esses argumentos usando as variáveis ​​específicas do Fish, como $argv e $argc. Aqui está um exemplo simples:
```

```
$ fish programa.fish arg1 arg2
```

```
O código acima irá ler os argumentos "arg1" e "arg2" e você pode acessá-los usando $argv[1] e $argv[2], respectivamente. Aqui está um exemplo de código para percorrer todos os argumentos passados ​​para o programa:
```

```
for arg in $argv
	echo $arg
end
```

## Profundando:

Ler argumentos da linha de comando é uma técnica que tem sido usada há décadas por programadores. Antes do Fish, outras linguagens de shell, como o Bash, também suportavam a leitura de argumentos da linha de comando. No entanto, o Fish tem uma sintaxe mais simples e intuitiva para acessar esses argumentos.

Existem alguns outros métodos para ler argumentos da linha de comando, como usando uma biblioteca externa ou implementando sua própria função de parsing. No entanto, o suporte integrado do Fish torna o processo mais fácil e rápido.

## Veja também:

- [Documentação oficial do Fish Shell](https://fishshell.com/docs/current/index.html)
- [Tutorial sobre como ler argumentos da linha de comando em Fish Shell](https://fishshell.com/docs/current/tutorial.html#tut_arguments)
- [Discussão sobre a leitura de argumentos da linha de comando no Fish Shell Github](https://github.com/fish-shell/fish-shell/issues/206)