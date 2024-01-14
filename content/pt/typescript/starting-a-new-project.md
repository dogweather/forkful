---
title:    "TypeScript: Começando um novo projeto"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Porquê iniciar um novo projeto? 

Iniciar um novo projeto em TypeScript pode ser uma ótima maneira de melhorar suas habilidades de programação e criar aplicações mais robustas. TypeScript é uma linguagem de programação baseada em JavaScript que adiciona recursos de tipagem estática e programação orientada à objeto. Com sua crescente popularidade, aprender TypeScript pode ser uma vantagem para qualquer desenvolvedor.

## Como começar

### Instalação e configuração

Antes de começar a escrever código em TypeScript, você precisará instalá-lo em sua máquina e configurar sua IDE preferida para suportar a linguagem. Para isso, você pode seguir as instruções no site oficial do TypeScript. 

### Criando um projeto

Para começar um novo projeto em TypeScript, você pode usar o gerenciador de pacotes npm ou o yarn. Primeiro, crie uma pasta para o projeto e navegue até ela no seu terminal. Em seguida, execute o seguinte comando:

```TypeScript
npm init -y
```

Isso criará um arquivo `package.json` para gerenciar as dependências do projeto. Em seguida, instale o TypeScript usando o seguinte comando:

```TypeScript
npm install typescript --save-dev
```

Isso instalará o TypeScript localmente no seu projeto. Agora, crie um arquivo `tsconfig.json` na raiz do seu projeto e configure-o para seus requisitos específicos. 

### Escrevendo o código

Agora que seu projeto está configurado, você pode começar a escrever código em TypeScript. Aqui está um exemplo simples de uma função que recebe como parâmetro dois números e retorna sua soma:

```TypeScript
const soma = (num1: number, num2: number): number => {
  return num1 + num2
}

const resultado = soma(5, 10)
console.log(resultado) // output: 15
```

Observe como os tipos `number` foram especificados nos parâmetros da função e no seu retorno. Isso é o que torna TypeScript uma linguagem de tipagem estática.

## Mergulho profundo

Além de adicionar tipos estáticos e recursos de POO ao JavaScript, TypeScript também possui outras vantagens, como suporte a módulos, interfaces e classes. Se você quiser se aprofundar mais em TypeScript, confira a documentação oficial e experimente diferentes recursos em seus projetos. 

## Veja também

- [Documentação oficial do TypeScript](https://www.typescriptlang.org/docs/)
- [Curso gratuito de TypeScript na Udemy](https://www.udemy.com/course/typescript-pt/)