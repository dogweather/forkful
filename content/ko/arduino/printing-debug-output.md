---
title:    "Arduino: 디버그 출력하기"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## 왜: 디버그 출력을 활용하는 이유
디버그 출력은 코드를 디버깅하는 데 매우 유용합니다. 실행 중인 프로그램의 상태를 실시간으로 확인할 수 있으며, 코드의 특정 부분에서 문제가 발생할 때 무엇이 잘못되었는지 이해하는데 도움이 됩니다.

## 사용 방법:
디버그 출력은 `Serial.print()` 함수를 사용하여 쉽게 구현할 수 있습니다. 아래의 코드 블록을 참고해보세요.

```Arduino
// 디버그 메시지를 출력하는 전면을 선언합니다.
#define DEBUG 1

void setup(){
  // 시리얼 통신을 시작합니다.
  Serial.begin(9600);

  // 디버그 모드가 활성화되어 있는지 확인합니다.
  #ifdef DEBUG
    // 디버그 메시지를 출력합니다.
    Serial.println("디버그 메시지 출력 중...");
  #endif
}

void loop(){
  // 코드 실행 중에 디버그 메시지를 출력할 수 있습니다.
  #ifdef DEBUG
    // 변수의 값을 출력해볼 수 있습니다.
    int num = 10;
    Serial.print("num = ");
    Serial.println(num);

    // 문자열도 출력할 수 있습니다.
    String hello = "안녕하세요!";
    Serial.println(hello);

    // 변수의 값을 실시간으로 확인할 수 있습니다.
    int val = analogRead(A0);
    Serial.print("A0 핀의 입력 값: ");
    Serial.println(val);
  #endif
}
```

위의 코드를 실행시키면 시리얼 모니터에 디버그 메시지가 출력될 것입니다.

## 딥 다이브:
디버그 출력은 복잡한 코드를 디버깅하는 데 유용하지만, 많은 메시지를 출력하면 시리얼 통신이 지연될 수 있습니다. 이를 해결하기 위해서는 `delay()` 함수를 사용하여 메시지를 출력하는 시간 간격을 조정할 수 있습니다. 또한 테스트가 끝난 후에는 디버그 모드를 비활성화하여 불필요한 출력을 방지할 수 있습니다.

## 참고 자료:
- [Arduino Reference](https://www.arduino.cc/reference/en/language/functions/communication/serial/print/)
- [Making Debugging Messages in Arduino Code Efficient](https://www.instructables.com/Making-Debugging-Messages-in-Arduino-Code-Efficien/)