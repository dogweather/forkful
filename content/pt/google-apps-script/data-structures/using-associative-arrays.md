---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:04:03.790061-07:00
description: "Arrays associativos, conhecidos como objetos no Google Apps Script (uma\
  \ variante do JavaScript), permitem que programadores criem cole\xE7\xF5es de pares\
  \ chave-\u2026"
lastmod: '2024-03-13T22:44:46.099392-06:00'
model: gpt-4-0125-preview
summary: "Arrays associativos, conhecidos como objetos no Google Apps Script (uma\
  \ variante do JavaScript), permitem que programadores criem cole\xE7\xF5es de pares\
  \ chave-\u2026"
title: Usando arrays associativos
weight: 15
---

## O quê & Por quê?

Arrays associativos, conhecidos como objetos no Google Apps Script (uma variante do JavaScript), permitem que programadores criem coleções de pares chave-valor. Essa funcionalidade é crucial para armazenar e manipular dados de forma eficiente, especialmente quando se trabalha com propriedades de nomes dinâmicos ou quando o modelo de armazenamento e acesso linear de um array tradicional é insuficiente.

## Como fazer:

No Google Apps Script, você cria e manipula arrays associativos (objetos) usando chaves `{}`, definindo pares chave-valor dentro delas. As chaves são identificadores únicos, e os valores podem ser qualquer coisa, desde strings e números até objetos mais complexos ou funções. Aqui está um exemplo básico:

```javascript
function createAssociativeArray() {
  var user = {
    name: "John Doe",
    age: 30,
    email: "johndoe@example.com"
  };

  // Acessando valores
  Logger.log(user.name); // Saída: John Doe
  Logger.log(user["email"]); // Saída: johndoe@example.com

  // Adicionando novos pares de chave-valor
  user.title = "Desenvolvedor de Software";
  user["country"] = "EUA";

  Logger.log(user.title); // Saída: Desenvolvedor de Software

  // Iterando sobre pares de chave-valor
  for (var key in user) {
    Logger.log(key + ': ' + user[key]);
  }
}
```

A saída da amostra para a parte da iteração pode parecer assim:
```
name: John Doe
age: 30
email: johndoe@example.com
title: Desenvolvedor de Software
country: EUA
```

Observe como você pode usar tanto a notação por ponto quanto a notação por colchetes para acessar e definir propriedades. A notação por colchetes é particularmente útil ao trabalhar com chaves que são determinadas dinamicamente ou incluem caracteres não permitidos em identificadores.

## Aprofundando

Arrays associativos na forma de objetos têm sido um pilar do JavaScript, e por extensão do Google Apps Script, refletindo seu mecanismo de herança baseado em protótipos. Ao contrário de linguagens com arrays associativos tradicionais ou dicionários (por exemplo, o dict do Python), os objetos do Google Apps Script oferecem um meio flexível e poderoso de estruturar dados, beneficiando-se da natureza dinâmica do JavaScript.

É importante notar, no entanto, que a especificação do ECMAScript 2015 introduziu objetos `Map` e `Set`, oferecendo um manuseio de coleções associativas mais direto com certos benefícios sobre objetos, como a manutenção da ordem de inserção e melhor desempenho para grandes conjuntos de dados. Embora o Google Apps Script também suporte esses, a escolha entre usar objetos ou as estruturas `Map`/`Set` mais novas depende de necessidades específicas e considerações de desempenho. Para a maioria das tarefas de array associativo, as implementações baseadas em objetos tradicionais fornecem uma abordagem familiar e versátil, mas é aconselhável examinar alternativas mais recentes à medida que a complexidade do seu script aumenta.
