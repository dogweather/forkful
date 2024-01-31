---
title:                "Trabalhando com JSON"
date:                  2024-01-19
html_title:           "Arduino: Trabalhando com JSON"
simple_title:         "Trabalhando com JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/working-with-json.md"
---

{{< edit_this_page >}}

## O Que é & Porquê?

Trabalhar com JSON (JavaScript Object Notation) é lidar com um formato leve de troca de dados. Programadores o usam por sua facilidade de leitura e escrita, e por ser facilmente interpretável por máquinas.

## Como Fazer:

```TypeScript
// Definindo uma interface para o tipo de dados
interface Usuario {
  id: number;
  nome: string;
  email: string;
}

// Uma string JSON exemplo
const jsonString: string = '{"id": 1, "nome": "João", "email": "joao@example.com"}';

// Convertendo de JSON para um objeto TypeScript
const usuarioObj: Usuario = JSON.parse(jsonString);
console.log(usuarioObj); // Saída: { id: 1, nome: 'João', email: 'joao@example.com' }

// Convertendo um objeto TypeScript para uma string JSON
const novoJsonString: string = JSON.stringify(usuarioObj);
console.log(novoJsonString); // Saída: '{"id":1,"nome":"João","email":"joao@example.com"}'
```

## Mergulho Profundo:

JSON, derivado da notação de objeto JavaScript, tornou-se um padrão de fato para troca de dados desde os anos 2000. Existem alternativas como XML, mas JSON predomina devido à sua simplicidade e compatibilidade com a web. A implementação em TypeScript é semelhante à do JavaScript, mas com o benefício adicional da verificação de tipos em tempo de compilação.

## Veja Também:

- MDN Web Docs sobre JSON: https://developer.mozilla.org/pt-BR/docs/Learn/JavaScript/Objects/JSON
- JSON.org: https://www.json.org/json-pt.html
- TypeScript Handbook: https://www.typescriptlang.org/docs/handbook/intro.html
