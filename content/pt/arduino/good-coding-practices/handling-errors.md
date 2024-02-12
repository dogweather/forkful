---
title:                "Tratamento de Erros"
aliases:
- /pt/arduino/handling-errors/
date:                  2024-01-26T00:37:06.996309-07:00
model:                 gpt-4-1106-preview
simple_title:         "Tratamento de Erros"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/handling-errors.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

O tratamento de erros em seus programas captura as coisas imprevistas que tentarão te atrapalhar. Você faz isso para evitar que seu Arduino entre em colapso quando o inesperado ocorrer.

## Como Fazer:

Digamos que seu Arduino esteja lendo um sensor que pode ocasionalmente produzir valores fora do intervalo. Aqui está como você pode lidar com isso:

```Arduino
int sensorValue = analogRead(A0);

if (sensorValue >= 0 && sensorValue <= 1023) {
  // O valor está dentro do intervalo, prossiga com o processamento
  Serial.println(sensorValue);
} else {
  // O valor está fora do intervalo, trate o erro
  Serial.println("Erro: Valor do sensor fora do intervalo.");
}
```
Saída de Exemplo:
```
523
Erro: Valor do sensor fora do intervalo.
761
```

## Aprofundamento

O tratamento de erros nem sempre foi tão direto. Nos primórdios, os desenvolvedores muitas vezes ignoravam os erros, levando ao temido "comportamento indefinido". Conforme a programação evoluiu, as ferramentas também — agora você tem exceções em muitas linguagens, mas elas ainda são uma boa e velha 'verificação prévia' no mundo Arduino devido às limitações de hardware e às raízes do C++.

Na programação Arduino, é comum ver instruções `if-else` para tratamento de erros. Mas existem alternativas: usar a função `assert` para interromper a execução se uma condição falhar ou projetar dispositivos de segurança dentro da própria configuração de hardware.

Ao implementar o tratamento de erros, considere o impacto de parar o programa versus permitir que ele continue com um estado padrão ou seguro. Há uma troca, e a escolha certa depende do potencial dano de interrupções versus operação incorreta.

## Veja Também

Aprimore-se na detecção e tratamento de erros com estes recursos:

- Referência da Linguagem Arduino: https://www.arduino.cc/reference/en/
- Uma análise mais profunda do tratamento de erros pela Embedded Artistry: https://embeddedartistry.com/blog/2017/05/17/creating-a-circular-buffer-in-c-and-c/
- Tratamento de Erros em C++: https://en.cppreference.com/w/cpp/error/exception

Isso deve lhe dar o conhecimento e a confiança para evitar as armadilhas dos erros em suas aventuras com Arduino.
