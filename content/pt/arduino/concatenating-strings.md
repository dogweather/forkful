---
title:                "Arduino: Concatenando strings"
simple_title:         "Concatenando strings"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## Porque

Ao escrever código em Arduino, às vezes pode ser necessário combinar duas ou mais strings para criar uma única string. Isso é conhecido como concatenar strings e pode ser útil em muitas situações, como exibir dados em um display ou enviar um texto em um módulo WiFi. 

## Como Fazer

Para concatenar strings em Arduino, primeiro é importante entender que as strings são armazenadas como arrays de caracteres. Isso significa que cada caractere em uma string tem um índice específico, começando em 0. Por exemplo, a string "Arduino" tem 7 caracteres e o índice do primeiro caractere (a) é 0.

Então, para concatenar duas strings, podemos usar a função `strcat` que une a segunda string à primeira string. Aqui está um exemplo de código que concatena duas strings e as exibe no monitor serial:

```Arduino
char string1[] = "Ola ";
char string2[] = "Arduino";
strcat(string1, string2);
Serial.println(string1);
```
A saída deste código será "Ola Arduino" no monitor serial.

No entanto, é importante lembrar que o array de caracteres da primeira string deve ter espaço suficiente para armazenar a segunda string. Caso contrário, podem ocorrer erros ou a concatenação não será bem-sucedida. Portanto, é essencial definir o tamanho máximo do array de caracteres ao declarar a primeira string.

## Mergulho Profundo

Além da função `strcat`, existem outras maneiras de concatenar strings em Arduino, como o uso do operador `+` ou a função `sprintf`. O operador `+` funciona de maneira semelhante à função `strcat`, mas é mais simples de usar. Já a função `sprintf` permite inserir valores em uma string formatada, o que pode ser útil em casos em que é necessário concatenar uma string com valores numéricos.

Outro aspecto importante a ser mencionado é que a concatenação de strings pode ser uma operação que requer mais memória e processamento do Arduino. Portanto, é importante considerar o tamanho máximo das strings e evitar concatenações desnecessárias para otimizar o desempenho do código.

## Veja Também
- [Documentação oficial do Arduino sobre strings](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [Tutorial de concatenação de strings para iniciantes](https://blog.arduino.cc/2019/01/16/concatenating-strings-for-beginners/) 
- [Mais sobre arrays de caracteres em Arduino](https://www.programmingelectronics.com/arduino-beginners-string-manipulation-using-arrays/)

Com essas informações, agora você está pronto para concatenar strings em seus projetos de Arduino. Lembre-se de sempre considerar as melhores práticas e a otimização do código para garantir um bom desempenho do seu dispositivo. Até a próxima!