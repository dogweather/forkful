---
title:    "Arduino: Verificando se um diretório existe"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

##Por que

Há muitos projetos de programação em que precisamos verificar se um diretório (ou pasta) existe antes de prosseguir com outras atividades. Isso pode ser necessário para garantir que um arquivo seja criado no local adequado ou para evitar erros em nosso código. Portanto, verificar a existência de um diretório é uma etapa importante na programação em Arduino.

##Como fazer

Para verificar se um diretório existe em um código Arduino, podemos usar a função `exists()` da biblioteca SD. Primeiro, precisamos incluir essa biblioteca em nosso sketch:

```Arduino
#include <SD.h>
```

Em seguida, podemos utilizar a função `exists()` passando o nome do diretório que queremos verificar como parâmetro. Por exemplo, se quisermos verificar se o diretório "meus_arquivos" existe, nosso código seria o seguinte:

```Arduino
if(SD.exists("meus_arquivos")){
  Serial.println("O diretório existe!");
} else {
  Serial.println("O diretório não existe!");
}
```

Se o diretório existir, a mensagem "O diretório existe!" será impressa no Monitor Serial. Caso contrário, a mensagem "O diretório não existe!" será exibida.

##Aprofundando

Quando usamos a função `exists()`, é importante lembrar que ela só verifica a existência de um diretório, e não de um arquivo. Para verificar se um determinado arquivo existe, podemos usar a função `open()`, também da biblioteca SD. Além disso, é importante mencionar que essa função só funciona se o cartão SD estiver presente e inicializado corretamente.

##Veja também

- [Tutorial: Como criar, ler e escrever em arquivos no Arduino](https://www.filipeflop.com/blog/arduino-criar-ler-escrever-arquivos-sd/)
- [Documentação oficial da biblioteca SD](https://www.arduino.cc/en/Reference/SD)
- [Guia completo sobre uso de cartões SD com Arduino](https://www.arduino.cc/en/Guide/ArduinoSD)