---
date: 2024-01-20 17:55:34.356535-07:00
description: "Como fazer: Rodando o script com `bash meuscript.sh arg1 arg2` ter\xED\
  amos esta sa\xEDda."
lastmod: '2024-04-05T21:53:47.115256-06:00'
model: gpt-4-1106-preview
summary: "Rodando o script com `bash meuscript.sh arg1 arg2` ter\xEDamos esta sa\xED\
  da."
title: Lendo argumentos da linha de comando
weight: 23
---

## Como fazer:
```Bash
#!/bin/bash
echo "Primeiro argumento: $1"
echo "Segundo argumento: $2"
echo "Todos os argumentos: $@"
echo "Número de argumentos: $#"
```

Rodando o script com `bash meuscript.sh arg1 arg2` teríamos esta saída:

```
Primeiro argumento: arg1
Segundo argumento: arg2
Todos os argumentos: arg1 arg2
Número de argumentos: 2
```

## Aprofundando:
Os argumentos da linha de comando são uma funcionalidade tão antiga quanto os próprios shells Unix. Embora existam outras ferramentas modernas como o `getopts` e programas externos como `argparse` para Python, a leitura direta dos parâmetros `$1`, `$2`, `$@`, `$*` e `$#` persiste devido à sua simplicidade e praticidade em scripts.

O `$1`, `$2`, são posições dos argumentos. `$@` e `$*` retornam todos argumentos, mas diferem quando usados entre aspas -`"$@"` mantém os argumentos separados, enquanto `"$*"` os considera uma string única. `$#` fornece o número total de argumentos passados. 

Uma implementação com `getopts` permite um manuseio mais sofisticado de opções com chaves (como `-a` ou `--argumento`), o que seria assim:

```Bash
#!/bin/bash
while getopts "a:b:" opt; do
  case $opt in
    a) echo "Opção A com valor $OPTARG" ;;
    b) echo "Opção B com valor $OPTARG" ;;
    \?) echo "Opção inválida: -$OPTARG" ;;
  esac
done
```

Com a execução `bash meuscript.sh -a umValor -b outroValor`, teremos:

```
Opção A com valor umValor
Opção B com valor outroValor
```

## Veja também:
- [Advanced Bash-Scripting Guide](https://tldp.org/LDP/abs/html/)
- [Bash Reference Manual](https://www.gnu.org/software/bash/manual/)
- [ExplainShell](https://explainshell.com/) - para explicar linhas de comando
- [ShellCheck](https://www.shellcheck.net/) - para encontrar erros em seus scripts Bash
