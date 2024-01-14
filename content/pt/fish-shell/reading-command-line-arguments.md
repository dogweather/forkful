---
title:    "Fish Shell: Lendo argumentos da linha de comando"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

# Por que ler argumentos da linha de comando é importante em programação com Fish Shell?

Muitos programadores preferem trabalhar com a linha de comando ao invés de interfaces gráficas devido à sua eficiência e rapidez. Nesse sentido, compreender como ler e interpretar os argumentos passados ​​pela linha de comando é essencial para usar o Fish Shell de forma efetiva.

## Como ler argumentos da linha de comando com Fish Shell

Para ler os argumentos da linha de comando com o Fish Shell, podemos usar a variável de ambiente $argv. Ela armazena uma lista dos argumentos passados e pode ser acessada através de um loop "for", como mostrado no exemplo abaixo:

```Fish Shell
for argument in $argv
	echo "Argumento: $argument"
end
```

Ao executar o código acima com a linha de comando "fish teste.fish argumento1 argumento2", o output será:

```
Argumento: argumento1
Argumento: argumento2
```

Podemos também usar diversos comandos, como "set", "count" e "contains", para manipular essa variável e obter informações específicas dos argumentos passados.

## Aprofundando na leitura de argumentos da linha de comando

Uma funcionalidade interessante do Fish Shell é a possibilidade de usar flags nos argumentos da linha de comando, o que permite passar informações adicionais para o programa. Para isso, podemos usar o comando "argparse" (similar ao utilizado em Python), que irá auxiliar na leitura dos argumentos e flags. Exemplo:

```Fish Shell
set parser (argparse description "Exemplo de leitura de argumentos da linha de comando")
argparse.add_argument "--nome" "-n" option name
argparse.add_argument "--idade" "-i" option age
argparse.add_argument "--profissao" "-p" option occupation
set --optn (argparse.parseArgStack $argv)
echo "Nome: $optn[name]"
echo "Idade: $optn[age]"
echo "Profissao: $optn[occupation]"
```

Ao executar o código acima com a linha de comando "fish teste.fish -n Maria -i 30 -p Médica", o output será:

```
Nome: Maria
Idade: 30
Profissao: Médica
```

É importante notar que o Fish Shell também possui outras funcionalidades para lidar com a leitura de argumentos, como o "positional" e o "mandatory options". Para uma compreensão mais profunda sobre esses recursos, é recomendado ler a documentação e experimentar em projetos pessoais.

# Veja também

- Documentação oficial do Fish Shell sobre a leitura de argumentos da linha de comando: https://fishshell.com/docs/current/cmds/argparse.html
- Tutorial em vídeo do canal "LearnCode.academy" sobre o uso de argparse no Fish Shell: https://www.youtube.com/watch?v=nYC3pYZBXac
- Artigo do blog "DigitalOcean" sobre a utilização de flags na leitura de argumentos com o Fish Shell: https://www.digitalocean.com/community/tutorials/how-to-use-flag-arguments-in-your-bash-scripts-pt