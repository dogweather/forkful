---
title:    "Arduino: Concatenando strings"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Porque

Ao programar em Arduino, é comum a necessidade de combinar ou juntar diferentes strings para criar uma mensagem ou comando completo. A concatenação de strings é uma habilidade essencial para qualquer programador, permitindo que você crie códigos mais complexos e eficientes.

## Como Fazer

Para concatenar strings em Arduino, você precisará usar a função `strcat()`. Esta função pega duas strings e as combina em uma só. Veja um exemplo abaixo:

```Arduino
char primeiraString[] = "Olá ";
char segundaString[] = "mundo!";
strcat(primeiraString, segundaString);
Serial.println(primeiraString);
```
Neste exemplo, a função `strcat()` combina as duas strings e a saída será "Olá mundo!".

É importante lembrar que, ao usar a função `strcat()`, a primeira string deve ter espaço suficiente para acomodar a segunda string. Caso contrário, pode ocorrer um erro no seu código.

Você também pode concatenar mais de duas strings usando a função `strcat()`. Basta colocar uma após a outra, como no exemplo abaixo:

```Arduino
char primeiraString[] = "Eu ";
char segundaString[] = "amo ";
char terceiraString[] = "programar!";
strcat(primeiraString, segundaString);
strcat(primeiraString, terceiraString);
Serial.println(primeiraString);
```
A saída do código acima será "Eu amo programar!".

## Mergulho Profundo

Ao concatenar strings em Arduino, é importante lembrar que todas elas devem estar em formato `char[]` ou `char *`. Se você estiver utilizando strings em formato `String`, será necessário convertê-las para `char[]` antes de realizar a concatenação.

Além disso, é recomendável que você evite o uso de `strcpy()` em conjunto com `strcat()`, pois essa combinação pode causar bugs e problemas de memória em seu código.

## Veja Também

- Documentação oficial da função `strcat()` em Arduino: https://www.arduino.cc/reference/pt/language/variables/data-types/string/functions/strcat/

- Tutorial sobre concatenação de strings em Arduino: https://www.arduino.cc/en/Tutorial/StringAdditionOperator/