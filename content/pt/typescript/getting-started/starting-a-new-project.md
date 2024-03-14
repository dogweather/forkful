---
date: 2024-01-20 18:04:31.744650-07:00
description: "Come\xE7ar um novo projeto \xE9 como abrir um livro em branco para um\
  \ programador: \xE9 uma oportunidade de criar algo do zero. Programadores come\xE7\
  am novos\u2026"
lastmod: '2024-03-13T22:44:46.326633-06:00'
model: gpt-4-1106-preview
summary: "Come\xE7ar um novo projeto \xE9 como abrir um livro em branco para um programador:\
  \ \xE9 uma oportunidade de criar algo do zero. Programadores come\xE7am novos\u2026"
title: Iniciando um novo projeto
---

{{< edit_this_page >}}

## O que é & Por que?
Começar um novo projeto é como abrir um livro em branco para um programador: é uma oportunidade de criar algo do zero. Programadores começam novos projetos para resolver problemas, explorar tecnologias novas, ou simplesmente por diversão.

## Como Fazer:
Vamos montar um projeto TypeScript básico. Primeiro, certifique-se de ter o Node.js e o npm instalados. Depois, instale o TypeScript globalmente:

```bash
npm install -g typescript
```

Agora, crie um diretório para o seu projeto e inicialize o npm:

```bash
mkdir meu-projeto-ts
cd meu-projeto-ts
npm init -y
```

Adicione um arquivo `tsconfig.json` para configurar o TypeScript:

```bash
tsc --init
```

Seu `tsconfig.json` básico será assim:

```json
{
  "compilerOptions": {
    "target": "es2016",
    "module": "commonjs",
    "outDir": "./dist",
    "strict": true,
    "esModuleInterop": true
  },
  "include": ["src/**/*"]
}
```

Crie um arquivo de exemplo TypeScript `src/index.ts`:

```typescript
function saudar(nome: string): string {
  return `Olá, ${nome}!`
}

console.log(saudar('mundo'));
```

Compile seu código TypeScript para JavaScript:

```bash
tsc
```

Você terá o arquivo `dist/index.js` gerado que pode ser executado com Node.js:

```bash
node dist/index.js
```

Saída esperada:

```plaintext
Olá, mundo!
```

## Aprofundando:

TypeScript foi criado pela Microsoft, lançado pela primeira vez em 2012. Oferece tipagem estática opcional para JavaScript, o que significa que você pode capturar muitos erros em tempo de compilação antes que seu código vá para produção.

Alternativas para o TypeScript incluem Flow do Facebook, mas TypeScript tem se destacado pela vasta adoção e suporte integrado em muitos editores de texto como o Visual Studio Code.

Quando você inicializa um projeto TypeScript com `tsc --init`, está criando um arquivo `tsconfig.json`. Esse arquivo controla como o compilador TypeScript funciona. Por exemplo, `target` define a versão ECMAScript para a saída do JavaScript, e `strict` ativa todas as opções estritas de checagem de tipo.

Você pode personalizar seu `tsconfig.json` para adequar-se ao seu projeto, definindo resolução de módulos, adicionando aliases para importações, entre outras otimizações.

## Veja Também:

- [Documentação Oficial do TypeScript](https://www.typescriptlang.org/docs/)
- [TypeScript no GitHub](https://github.com/Microsoft/TypeScript)
- [Porque TypeScript? - Uma visão geral](https://medium.com/@basarat/why-typescript-77a9de7bfb38)
- [Compilador de TypeScript online](https://www.typescriptlang.org/play)
