---
date: 2024-01-26 00:58:48.154449-07:00
description: "Como fazer: O Arduino n\xE3o vem com uma biblioteca de registro integrada\
  \ como alguns outros ambientes, mas voc\xEA pode implementar um registro b\xE1sico\
  \ no\u2026"
lastmod: '2024-03-13T22:44:46.845208-06:00'
model: gpt-4-1106-preview
summary: "O Arduino n\xE3o vem com uma biblioteca de registro integrada como alguns\
  \ outros ambientes, mas voc\xEA pode implementar um registro b\xE1sico no console\
  \ Serial com pouco esfor\xE7o."
title: Registro de Logs
weight: 17
---

## Como fazer:
O Arduino não vem com uma biblioteca de registro integrada como alguns outros ambientes, mas você pode implementar um registro básico no console Serial com pouco esforço. Aqui está um exemplo rápido para começar:

```arduino
void setup() {
  // Inicia a comunicação serial com a taxa de bauds especificada
  Serial.begin(9600);

  // Aguarde a conexão da porta serial - necessário apenas em algumas placas
  while (!Serial) {
    ; // espera pela conexão da porta serial. Necessário para USB nativo
  }

  // Registre uma mensagem informativa indicando que o processo de configuração está completo
  Serial.println("Configuração completa!");
}

void loop() {
  // Registrador simples que imprime o tempo de funcionamento a cada segundo
  static unsigned long lastLogTime = 0;
  unsigned long currentMillis = millis();

  if (currentMillis - lastLogTime >= 1000) {
    lastLogTime = currentMillis;
    Serial.print("Tempo ativo (ms): ");
    Serial.println(currentMillis);

    // Aqui você também poderia adicionar registros de erros, avisos ou outras informações.
  }
  
  // Resto da lógica do seu programa aqui...
}
```

Saída de Amostra do Serial:
```
Configuração completa!
Tempo ativo (ms): 1000
Tempo ativo (ms): 2000
Tempo ativo (ms): 3000
...
```

## Aprofundamento:
Historicamente, o registro em microcontroladores não era tão direto quanto em um sistema operacional completo. Recursos limitados significavam que cada byte contava, e os desenvolvedores precisavam ter cuidado para não congestionar o sistema. Com o advento de placas mais capazes e a simplificação do processo pela plataforma Arduino, o registro se tornou mais acessível.

Embora o código acima demonstre o registro via interface Serial, outros métodos incluem gravação em um cartão SD, enviando dados pela rede para um servidor remoto ou mesmo exibindo em um pequeno LCD.

Implementar um sistema de registro traz considerações, tais como rotação, severidade de nível (informação, depuração, aviso, erro) e impacto no desempenho. No Arduino, pode ser necessário estar atento às restrições de memória ao registrar estruturas de dados complexas. Para registros remotos, a segurança dos registros transmitidos também é uma preocupação.

Soluções mais sofisticadas, como o Syslog, um padrão de registro amplamente adotado, existem fora do mundo do Arduino, mas você pode integrar bibliotecas de terceiros que oferecem funcionalidade semelhante com vários graus de complexidade e requisitos de recursos.

## Veja Também:
- [Referência `Serial` do Arduino](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [Registro com cartão SD no Arduino](https://www.arduino.cc/en/Tutorial/LibraryExamples/Datalogger)
- [Shield de Registro de Dados do SparkFun](https://www.sparkfun.com/products/13712)
- [TinyWeb: Um exemplo prático de registro remoto com Arduino](https://www.arduino.cc/en/Tutorial/WebClientRepeating)
