---
title:                "Arduino: 디버그 출력 출력"
simple_title:         "디버그 출력 출력"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

## 왜
Arduino 프로그래밍에서 디버그 출력을 하는 이유는 코드의 동작을 이해하고 문제를 해결하기 위해서입니다.

## 사용 방법
Arduino에 내장된 Serial 라이브러리를 이용하여 디버그 출력을 할 수 있습니다. 아래의 코드는 analogRead() 함수를 이용하여 A0 핀에서 읽어온 값과 그에 해당하는 전압 값을 출력하는 예시입니다.

```Arduino
void setup() {
  // 시리얼 통신 시작
  Serial.begin(9600);
}

void loop() {
  // A0 핀에서 값을 읽어옴
  int value = analogRead(A0);

  // Serial.print()를 이용하여 디버그 출력을 함
  Serial.print("A0 핀에서 읽은 값: ");
  Serial.print(value);
  Serial.print(", 해당하는 전압 값: ");
  Serial.print(value * (5.0 / 1023.0));
  Serial.println(" V");

  delay(1000);
}
```

위의 코드를 실행하면 아래와 같은 결과가 시리얼 모니터에 출력됩니다.

```
A0 핀에서 읽은 값: 768, 해당하는 전압 값: 1.49 V
```

## 깊이 파고들기
디버그 출력을 이용해 코드의 동작을 확인하는 것은 디버깅을 위해 꼭 필요한 과정입니다. 디버그 출력을 잘 활용하면 코드의 어떤 부분에서 오류가 발생하는지 쉽게 파악할 수 있습니다. 또한, 디버그 출력을 통해 변수의 값이 어떻게 변하는지를 확인할 수 있어 코드 분석에도 도움이 됩니다.

## See Also
- [Arduino Serial 라이브러리 공식 문서 (영어)](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [Serial.print() 함수를 이용한 디버그 출력 예제 (영어)](https://www.arduino.cc/en/Tutorial/LibraryExamples/SerialOutput)
- [Serial.println() 함수를 이용한 디버그 출력 예제 (영어)](https://howtomechatronics.com/examples/arduino-serial-print-function-tutorial/)