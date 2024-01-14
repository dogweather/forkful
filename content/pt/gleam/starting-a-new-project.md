---
title:    "Gleam: Começando um novo projeto"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Por que começar um novo projeto em Gleam?

Se você está procurando uma linguagem de programação funcional moderna e segura para criar seu próximo projeto, Gleam pode ser a escolha ideal. Com um sistema de tipos forte e orientado a objetos, além de uma grande comunidade de desenvolvedores, Gleam oferece uma ótima opção para criar aplicações escaláveis e confiáveis.

## Como fazer

Para começar, você precisará instalar o compilador e o runtime de Gleam em sua máquina. Você pode encontrar instruções detalhadas de instalação em seu site oficial. Depois de instalá-los, você pode iniciar um novo projeto em Gleam criando um novo diretório e dentro dele, um arquivo chamado "mix.exs". Dentro deste arquivo, você pode definir as dependências para o seu projeto, incluindo módulos e bibliotecas externas. Em seguida, você pode criar um arquivo "main.gleam" para escrever seu código.

Abaixo, segue um exemplo de código que imprime uma mensagem na tela:

```Gleam
import gleam/io

fn hello_world() {
  io.print("Olá, mundo!")
}

hello_world()
```

Ao executar este código, você verá a seguinte saída: "Olá, mundo!". A partir daí, você pode começar a escrever seu próprio código para criar uma aplicação mais complexa.

## Examinando mais a fundo

Gleam é uma linguagem fortemente tipada, o que significa que todos os valores têm um tipo específico e devem estar em conformidade com ele. Isso torna a depuração mais fácil e reduz a ocorrência de erros no código. No entanto, pode haver momentos em que você precise explicitamente desativar a verificação de tipos, por exemplo, ao trabalhar com JSON. Para isso, você pode usar o operador de exclusão `!!` antes de um valor.

Além disso, Gleam oferece uma rica variedade de ferramentas de depuração, testes e documentação para ajudá-lo a criar projetos de alta qualidade. Com a ajuda dessas ferramentas, você pode garantir que seu código esteja funcionando corretamente e com ótimo desempenho.

## Veja também

Aqui estão alguns links úteis para você explorar mais sobre Gleam e seus recursos:

- [Site oficial de Gleam](https://gleam.run)
- [Repositório oficial do GitHub](https://github.com/gleam-lang/gleam)
- [Documentação da linguagem](https://gleam.run/book/introduction.html)
- [Tutoriais e exemplos de código](https://gleam.run/tutorials/)