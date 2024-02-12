---
title:                "Iniciando um novo projeto"
aliases:
- /pt/bash/starting-a-new-project.md
date:                  2024-01-20T18:03:01.178678-07:00
model:                 gpt-4-1106-preview
simple_title:         "Iniciando um novo projeto"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/starting-a-new-project.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Começar um novo projeto é dar o pontapé inicial no desenvolvimento de uma ideia de software. Programadores iniciam projetos para transformar conceitos em realidade funcional, resolver problemas ou simplesmente para aprender e se divertirem com novos desafios.

## Como Fazer:

Para começar um projeto em Bash, primeiro criamos uma pasta e, em seguida, um script inicial. Aqui está o prático:

```Bash
mkdir meu_novo_projeto
cd meu_novo_projeto
touch meu_script.sh
echo '#!/bin/bash' > meu_script.sh
echo 'echo Olá, mundo!' >> meu_script.sh
chmod +x meu_script.sh
./meu_script.sh
```

Saída esperada:

```
Olá, mundo!
```

## Mergulho Profundo:

Bash é o acrônimo para "Bourne Again SHell", e é uma melhora direta do shell Bourne (sh), criado por Steve Bourne. Há outras alternativas de shell como Zsh e Fish que oferecem funcionalidades diferentes, mas Bash é conhecido por sua portabilidade e amplo uso. Quando começamos um novo projeto em Bash, focamos em criar um ambiente isolado onde a lógica e os comandos podem ser testados e desenvolvidos. Usualmente, isso envolve escrever scripts, definir variáveis de ambiente e configurar permissões. A decisão sobre como estruturar pastas e arquivos depende do escopo e da complexidade do projeto.

## Veja Também:

- [Bash Scripting Tutorial](https://linuxconfig.org/bash-scripting-tutorial)
- [Linux Command Line Basics](https://www.udacity.com/course/linux-command-line-basics--ud595)
- [The Bash Academy](http://guide.bash.academy/)
- [Advanced Bash-Scripting Guide](http://www.tldp.org/LDP/abs/html/)
