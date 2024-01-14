---
title:    "Javascript: Buscando e substituindo texto"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que alguém se envolveria em procurar e substituir texto?

Há muitos motivos pelos quais alguém pode precisar realizar uma busca e substituição de texto em seu código JavaScript. Pode ser necessário corrigir erros de digitação, atualizar nomes de variáveis, ou até mesmo alterar trechos inteiros de código. Usando as ferramentas certas, esse processo pode ser automatizado e economizar tempo e esforço durante o desenvolvimento.

## Como fazer uma busca e substituição em JavaScript

Para realizar uma busca e substituição de texto em JavaScript, existem algumas opções diferentes, dependendo do ambiente de codificação que você estiver usando. Aqui estão alguns exemplos de como realizar esse processo em diferentes ambientes:

### Usando o método replace() nativo do JavaScript:

```
let texto = "Olá, mundo!";
let novoTexto = texto.replace("mundo", "JavaScript");

console.log(novoTexto); // output: Olá, JavaScript!
```

### Usando o editor de código Visual Studio Code:

1. Pressione `Ctrl+Shift+H` (Windows) ou `Cmd+Shift+H` (Mac) para abrir a janela de busca.
2. Na barra lateral, clique no ícone de engrenagem e selecione "Substituir em arquivos" para abrir a ferramenta de busca e substituição.
3. Escolha o texto que deseja procurar e o texto que deseja substituir, selecione os arquivos nos quais deseja realizar a busca e clique em "Substituir Tudo".

### Usando a ferramenta de linha de comando Sed:

```
sed -i 's/mundo/JavaScript/g' arquivo.js
```

## Análise detalhada da busca e substituição de texto

Existem algumas coisas que devemos ter em mente ao realizar uma busca e substituição de texto em JavaScript. Primeiro, é importante notar que o método replace() nativo de JavaScript só substituirá a primeira ocorrência do texto que você estiver procurando. Se você quiser substituir todas as ocorrências, precisará adicionar a flag `g` no final da expressão regular.

Além disso, é importante entender o conceito de expressões regulares (regex) ao realizar uma busca e substituição de texto. As expressões regulares são padrões de texto usados para buscar e extrair informações de uma string. Elas são muito úteis em tarefas como buscar e substituir texto, já que elas permitem especificar padrões de texto mais complexos do que apenas uma correspondência exata.

Uma ferramenta muito útil para testar e aprender sobre expressões regulares é o [Regex101](https://regex101.com/), que permite inserir sua expressão regular e testá-la em tempo real em uma string.

## Veja também

- [Documentação do método replace() em JavaScript](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [Tutorial sobre expressões regulares em JavaScript](https://www.freecodecamp.org/news/a-guide-to-regular-expressions-in-javascript-82c6d30258e7/)
- [Tutorial de busca e substituição em Visual Studio Code](https://code.visualstudio.com/docs/editor/findandreplace)