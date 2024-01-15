---
title:                "디버그 출력 출력하기"
html_title:           "Arduino: 디버그 출력 출력하기"
simple_title:         "디버그 출력 출력하기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

## 왜

이해하기 쉬운 디버그 정보를 출력할 수 있어서 개발자들은 더 쉽게 코드를 디버깅할 수 있고, 문제를 해결할 수 있습니다.

## 방법

디버그 출력을 한 줄에 출력하는 방법:

```Arduino
Serial.println("Hello World!"); 
```

여러 개의 변수를 출력하는 방법:

```Arduino 
int num = 5; 
float decimal = 3.14; 
String text = "Arduino";

Serial.print("Number: "); 
Serial.println(num); 
Serial.print("Decimal: "); 
Serial.println(decimal); 
Serial.print("Text: "); 
Serial.println(text); 
```

센서 값을 출력하는 방법:

```Arduino
int sensorValue = analogRead(A0); 
Serial.print("Sensor Value: "); 
Serial.println(sensorValue);
```

## 깊은 고민

디버그 출력을 위해서는 `Serial` 라이브러리를 사용해야 합니다. 이 라이브러리는 USB 시리얼 포트를 통해 컴퓨터와 통신하게 해줍니다. 디버그 정보를 출력하고 싶은 함수 내부에서 `Serial.begin()`을 사용해야 합니다. 또한 `Serial`에서는 `println()`과 `print()` 함수를 사용하여 디버그 정보를 출력할 수 있습니다. 디버그 정보를 출력한 후에는 `Serial.end()` 함수를 사용하여 시리얼 통신을 끝내야 합니다.

## 관련 정보

📚 [Arduino Reference - Serial](https://www.arduino.cc/reference/en/language/functions/communication/serial/)

📚 [How to Use Serial Print in Arduino](https://www.makerspaces.com/how-to-use-serial-print-in-arduino/)