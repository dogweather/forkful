---
title:                "Gleam: Começando um novo projeto"
programming_language: "Gleam"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Por que começar um novo projeto em Gleam

A programação funcional tem sido uma das tendências mais populares no mundo da tecnologia nos últimos anos. E com isso, muitos programadores estão buscando novas ferramentas para incorporar em seus projetos. Gleam é uma linguagem de programação funcional recente, projetada para ser pura, expressiva e eficiente. Começar um novo projeto em Gleam pode ser uma ótima maneira de experimentar uma abordagem diferente e avançar suas habilidades de programação.

## Como iniciar um projeto Gleam

Começar um novo projeto Gleam é simples. Primeiro, você precisará ter o compilador Gleam instalado em sua máquina. Em seguida, basta seguir estes passos:

1. Crie uma nova pasta para o projeto e navegue até ela em seu terminal.
2. Inicie um novo projeto executando o seguinte comando: `gleam new nome_do_projeto`
3. Este comando criará uma estrutura básica para o seu projeto, incluindo um arquivo `gleam.toml` com as dependências do projeto e um arquivo `src` para o seu código fonte.
4. Agora, você pode começar a escrever seu código em Gleam e executá-lo usando `gleam build` e `gleam run`.
5. Para adicionar dependências ao seu projeto, basta editar o arquivo `gleam.toml` e usar o comando `gleam get` para baixá-las.

Aqui está um exemplo de código básico em Gleam que imprime "Olá Mundo!" no console:

```gleam
// Função main
pub fn main() {
    // Utilize a função `io.println` para imprimir no console
    io.println("Olá Mundo!")
}
```

Ao executar o código acima, você verá a saída "Olá Mundo!" no seu terminal.

## Mergulho profundo

Além da sintaxe simples e expressiva, começar um novo projeto em Gleam também traz vantagens técnicas. Gleam é uma linguagem compilada, o que significa que seu código é traduzido para um código de máquina altamente otimizado. Isso torna os programas escritos em Gleam mais eficientes e rápidos do que as linguagens interpretadas como Python ou JavaScript.

Outro aspecto interessante de Gleam é que ela é uma linguagem tipada de forma estática, o que significa que a verificação de tipos é feita durante o processo de compilação. Isso elimina muitos erros comuns encontrados em linguagens dinamicamente tipadas, tornando o código mais robusto e confiável.

Além disso, Gleam é projetada para ser altamente escalável e fácil de entender através de seu sistema de módulos. Com ele, é possível dividir seu código em diferentes módulos, promovendo uma melhor organização e reutilização de código em seu projeto.

Se você estiver interessado em aprender mais sobre como iniciar um projeto em Gleam e seus recursos avançados, recomendamos a leitura da documentação oficial e a experimentação com alguns projetos de exemplo.

## Veja também

- [Site oficial de Gleam](https://gleam.run/)
- [Documentação oficial de Gleam](https://gleam.run/documentation)
- [Código fonte de Gleam no GitHub](https://github.com/gleam-lang/gleam)