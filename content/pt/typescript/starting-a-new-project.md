---
title:    "TypeScript: Iniciando um novo projeto"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Por que começar um novo projeto em TypeScript?

Começar um novo projeto em TypeScript traz diversos benefícios para desenvolvedores. Com a tipagem estática e a possibilidade de utilizar recursos modernos do JavaScript, como classes e interfaces, TypeScript torna o processo de desenvolvimento mais organizado e eficiente.

## Como começar um projeto em TypeScript

Para começar um projeto em TypeScript, você precisa primeiro ter o TypeScript instalado em seu computador. Você pode fazer isso utilizando o gerenciador de pacotes npm, através do comando `npm install -g typescript`.

Depois de ter o TypeScript instalado, crie uma nova pasta para o seu projeto e navegue até ela no terminal. Em seguida, utilize o comando `tsc --init` para gerar um arquivo de configuração para o TypeScript.

Agora você pode começar a escrever seu código em TypeScript dentro de um arquivo com a extensão `.ts`. Para compilar seu código em JavaScript, utilize o comando `tsc nomeDoArquivo.ts`. O resultado será um arquivo `.js` com o mesmo nome, contendo seu código em JavaScript.

Para utilizar recursos do JavaScript moderno, como módulos e classes, você pode utilizar o compilador com a flag `--module` setada para `es2015` e a flag `--target` setada para `es5`.

Abaixo temos um exemplo de código TypeScript que utiliza classes e interfaces:

```TypeScript
// Classe Pessoa
class Pessoa {
  nome: string;
  idade: number;

  // Método construtor
  constructor(nome: string, idade: number) {
    this.nome = nome;
    this.idade = idade;
  }

  // Método para imprimir informações da pessoa
  imprimirInfo(): void {
    console.log(`Nome: ${this.nome}\nIdade: ${this.idade}`);
  }
}

// Interface para definir uma função
interface FuncaoPessoa {
  (pessoa: Pessoa): void;
}

// Função que recebe um objeto do tipo Pessoa e chama o método imprimirInfo
const imprimirPessoa: FuncaoPessoa = (pessoa: Pessoa): void => {
  pessoa.imprimirInfo();
};

// Criando um objeto do tipo Pessoa e chamando a função imprimirPessoa
const pessoa1: Pessoa = new Pessoa("João", 20);
imprimirPessoa(pessoa1);
```

A saída desse código será:

`Nome: João
Idade: 20`

## Mergulhando mais fundo

O TypeScript oferece muitos recursos poderosos para desenvolvedores. Além da tipagem estática e dos recursos modernos do JavaScript, TypeScript também permite a criação de módulos, interfaces genéricas e sobrecarga de funções.

Outra vantagem de começar um projeto em TypeScript é que ele é totalmente compatível com JavaScript. Isso significa que você pode utilizar bibliotecas e frameworks JavaScript existentes em seus projetos em TypeScript.

Para saber mais sobre os recursos do TypeScript, você pode consultar a documentação oficial em https://www.typescriptlang.org/docs/.

## Veja também

Aqui estão alguns links úteis para continuar aprendendo e explorando o mundo do TypeScript:

- Documentação oficial do TypeScript (https://www.typescriptlang.org/docs/)
- Tutorial do TypeScript no site da MDN (https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/TypeScript)
- Curso Interativo de TypeScript no Codecademy (https://www.codecademy.com/learn/learn-typescript)