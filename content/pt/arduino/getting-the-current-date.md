---
title:    "Arduino: Obtendo a data atual"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

# Por que obter a data atual é importante para sua programação Arduino?

Obter a data atual é um aspecto importante na programação Arduino, pois permite que você rastreie eventos e adicione funcionalidades baseadas em tempo ao seu projeto. Além disso, muitos sensores e dispositivos externos requerem que a data e a hora sejam sincronizadas para funcionar corretamente.

## Como obter a data atual em Arduino

Para obter a data atual no seu projeto Arduino, você precisará adicionar o módulo de tempo (Time.h) à sua biblioteca. Em seguida, você pode usar a função `now()` para obter a data e hora atuais em segundos. A partir daí, você pode formatar esse valor em componentes individuais, como dia, mês, ano e hora.

```arduino
#include <Time.h>

// Declaração de variáveis para armazenar a data atual
int dia, mes, ano;
int hora, minuto, segundo;

void setup() {
  // Inicialização da porta serial para visualizar a saída
  Serial.begin(9600);

  // Definindo a hora e data atual a partir da função `now()`
  time_t atual = now();

  // Formatando os valores em componentes individuais
  dia = day(atual);
  mes = month(atual);
  ano = year(atual);

  hora = hour(atual);
  minuto = minute(atual);
  segundo = second(atual);

  // Imprimindo a data e hora atuais na porta serial
  Serial.print("A data atual é: ");
  Serial.print(dia);
  Serial.print("/");
  Serial.print(mes);
  Serial.print("/");
  Serial.println(ano);
  Serial.print("A hora atual é: ");
  Serial.print(hora);
  Serial.print(":");
  Serial.print(minuto);
  Serial.print(":");
  Serial.println(segundo);
}

void loop() {
  // Seu código aqui
}
```

A saída dos componentes será algo parecido com isso:

```
A data atual é: 12/06/2021
A hora atual é: 14:30:05
```

## Mergulho profundo: entendendo a função `now()`

A função `now()` retorna a data e hora atual em segundos desde 1 de janeiro de 1970. Isso é conhecido como "Tempo Unix" e é amplamente utilizado em sistemas operacionais para rastrear eventos e executar agendamentos. Esta função pode ser usada para definir o tempo atual ou para compará-lo com outros eventos no seu código.

É importante lembrar que a função `now()` depende do relógio interno do Arduino. Se esse relógio não estiver configurado ou precisar ser atualizado, a data e hora retornadas podem ser imprecisas. Certifique-se de verificar a precisão do relógio interno em casos como esse.

# Veja também
- [Documentação do módulo Time.h](https://www.arduino.cc/reference/en/libraries/time/)
- [Funções de tempo do Arduino](https://www.arduino.cc/en/Tutorial/Foundations/DateTime)
- [Como sincronizar o relógio interno do Arduino](https://www.arduino.cc/en/Tutorial/SketchWithInterfaces)