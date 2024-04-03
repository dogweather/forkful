---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:00:21.113822-07:00
description: "Como fazer: O Google Apps Script, sendo uma linguagem baseada em JavaScript,\
  \ oferece m\xE9todos padr\xE3o para arredondar n\xFAmeros. Aqui est\xE1 uma explica\xE7\
  \xE3o de\u2026"
lastmod: '2024-03-13T22:44:46.101562-06:00'
model: gpt-4-0125-preview
summary: "O Google Apps Script, sendo uma linguagem baseada em JavaScript, oferece\
  \ m\xE9todos padr\xE3o para arredondar n\xFAmeros."
title: "Arredondamento de n\xFAmeros"
weight: 13
---

## Como fazer:
O Google Apps Script, sendo uma linguagem baseada em JavaScript, oferece métodos padrão para arredondar números. Aqui está uma explicação de três técnicas comumente usadas:

### Math.round()
Esta função arredonda um número para o inteiro mais próximo.

```javascript
var number = 2.56;
var roundedNumber = Math.round(number); 
Logger.log(roundedNumber); // Resultado: 3
```

### Math.ceil()
Arredonda um número para o inteiro mais próximo acima.

```javascript
var number = 2.56;
var roundedUp = Math.ceil(number); 
Logger.log(roundedUp); // Resultado: 3
```

### Math.floor()
Contrariamente, arredonda um número para o inteiro mais próximo abaixo.

```javascript
var number = 2.56;
var roundedDown = Math.floor(number); 
Logger.log(roundedDown); // Resultado: 2
```

Para casas decimais específicas, você pode usar `.toFixed()`, que na verdade retorna uma string, ou uma abordagem mais matizada para arredondamento:

```javascript
var number = 2.56789;
var fixedNumber = number.toFixed(2); 
Logger.log(fixedNumber); // Resultado: "2.57" (como uma string)

var preciseRound = Math.round(number * 100) / 100; 
Logger.log(preciseRound); // Resultado: 2.57
```

## Aprofundamento
Arredondar números no Google Apps Script não desvia muito de como é feito em outros ambientes JavaScript. No entanto, compreender as diferenças nos métodos de arredondamento e o potencial para problemas aritméticos de ponto flutuante é crucial. Por exemplo, devido à forma como os computadores representam floats, nem todas as frações decimais podem ser representadas com precisão perfeita, levando a resultados de arredondamento às vezes inesperados.

Historicamente, o JavaScript (e, por extensão, o Google Apps Script) lida com isso, conformando-se ao padrão IEEE 754, utilizado por muitas outras linguagens de programação para aritmética de ponto flutuante. Este padrão define como os números são arredondados, garantindo consistência em várias plataformas e idiomas.

Enquanto métodos diretos de arredondamento no Google Apps Script são simples e frequentemente suficientes, aplicações complexas ou de alta precisão podem se beneficiar de bibliotecas como decimal.js ou big.js, projetadas para lidar com aritmética de precisão arbitrária. Elas podem ser especialmente úteis quando se trabalha com cálculos financeiros ou científicos onde a precisão dos números arredondados é primordial.

Lembre-se, porém, que o uso de bibliotecas externas no Google Apps Script exige que sejam carregadas através do editor de scripts, o que pode introduzir dependências ou afetar o desempenho do seu script dependendo de como é utilizado. Em muitos casos, os métodos Math integrados são totalmente adequados, mas para aqueles casos limite que exigem precisão até o último grau, procurar além da biblioteca padrão pode ser necessário.
