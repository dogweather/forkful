---
title:                "Iniciando um novo projeto"
html_title:           "Gleam: Iniciando um novo projeto"
simple_title:         "Iniciando um novo projeto"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Por quê

Se você é um programador em busca de uma linguagem moderna, segura, com tipagem estática e eficiente, Gleam é a opção perfeita para você. Com uma sintaxe simples e uma comunidade acolhedora, Gleam é a escolha certa para iniciar seu próximo projeto.

## Como começar

Para começar a usar Gleam, é necessário ter o compilador instalado em seu sistema. Você pode baixá-lo facilmente através do gerenciador de pacotes ou diretamente do site oficial do Gleam.

Uma vez instalado, é hora de criar o seu primeiro projeto. Você pode fazer isso através do comando "```Gleam new my_project```", onde "my_project" é o nome que você deseja dar ao seu projeto. Isso criará uma estrutura básica para seu projeto, incluindo um arquivo "```main.gleam```" para o código principal.

Agora é só abrir o arquivo no seu editor de texto favorito e começar a programar em Gleam! Aqui está um exemplo simples de código que imprime "Olá mundo!" na tela:

```
Gleam import std/io

fn main() {
  io.println("Olá mundo!")
}
```

## Investigação aprofundada

Ao iniciar um novo projeto em Gleam, é importante entender a estrutura básica do código da linguagem. O código é dividido em módulos, que contêm funções e tipos de dados. Os módulos podem ser importados e usados em outros módulos.

Além disso, Gleam é fortemente tipado e usa inferência de tipos para determinar automaticamente o tipo de uma variável. Isso torna a linguagem segura e evita erros comuns durante a execução.

Outro aspecto interessante sobre Gleam é o sistema de tipos de dados personalizados, que permite a criação de tipos personalizados de acordo com as necessidades do seu projeto. Isso torna a linguagem muito flexível e adaptável a diferentes situações.

## Veja também

- Site oficial do Gleam: https://gleam.run/
- Documentação oficial: https://gleam.run/book/index.html
- Exemplos de código: https://github.com/gleam-lang/gleam/tree/master/examples