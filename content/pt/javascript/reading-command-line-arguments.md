---
title:                "Javascript: Lendo argumentos de linha de comando"
simple_title:         "Lendo argumentos de linha de comando"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por que

Você já se perguntou como os programas conseguem receber informações do usuário através da linha de comando? Isso é possível graças aos argumentos de linha de comando. Aprender a ler esses argumentos pode expandir suas habilidades de programação e tornar seus programas mais interativos.

## Como fazer

Para começar a ler os argumentos da linha de comando em Javascript, precisamos acessar o objeto `process`. Podemos usar a função `process.argv` para obter uma lista com os argumentos fornecidos pelo usuário. Vamos ver um exemplo simples:

```Javascript
// lendo argumentos da linha de comando
let args = process.argv;
console.log(args);
```

Se executarmos este código no terminal usando o comando `node`, o resultado será uma lista com os argumentos fornecidos, sendo o primeiro elemento o caminho para o arquivo de execução e os demais os argumentos fornecidos pelo usuário. Por exemplo, se executarmos o seguinte comando:

```
node index.js argumento1 argumento2 argumento3
```

o resultado seria:

```
[ 'caminho/para/o/arquivo/index.js', 'argumento1', 'argumento2', 'argumento3' ]
```

A partir dessa lista, você pode acessar cada argumento individualmente usando seu índice, por exemplo `args[2]` retornaria `argumento2`. Dessa forma, você pode utilizar informações fornecidas pelo usuário em seu programa Javascript.

## Mergulho profundo

Além da lista de argumentos, o objeto `process` também oferece outras informações úteis, como `process.argv0` (caminho absoluto para o executável Node.js), `process.cwd()` (caminho para o diretório de trabalho atual) e `process.env` (variáveis de ambiente). É importante ressaltar que os argumentos fornecidos sempre serão do tipo string, portanto, se precisar de um tipo diferente, como um número, será necessário convertê-lo.

## Veja também

- [Documentação do objeto Process no Node.js](https://nodejs.org/dist/latest-v14.x/docs/api/process.html)
- [Tutorial de leitura de argumentos da linha de comando em Javascript](https://www.digitalocean.com/community/tutorials/how-to-read-command-line-arguments-in-node-js)
- [Vídeo explicando como ler argumentos da linha de comando em Javascript](https://www.youtube.com/watch?v=o_fTUchHBPs)

Agora que você sabe como ler argumentos da linha de comando em Javascript, experimente implementar essa funcionalidade em seus próximos projetos e veja como ela pode melhorar a interatividade dos seus programas. Divirta-se programando!