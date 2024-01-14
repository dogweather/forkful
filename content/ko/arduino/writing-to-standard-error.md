---
title:                "Arduino: 표준 에러로 쓰기"
simple_title:         "표준 에러로 쓰기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# 왜 Writing to Standard Error가 필요한가요?

문제를 디버그하는 동안 또는 프로그램의 오류를 확인할 때, 추가적인 정보가 필요할 수 있습니다. 이때 에러 메시지는 프로그램에서 발견된 문제에 대한 정보를 제공해줄 수 있습니다. 이러한 경우, 표준 출력보다는 표준 에러를 사용하는 것이 더 적합합니다. 결론적으로, 오류 메시지를 표준 에러로 출력하는 것은 프로그램 디버깅과 오류 해결을 더 쉽고 효율적으로 만들어줍니다.

## 어떻게 Writing to Standard Error를 할 수 있나요?

아래의 코드를 따라해보세요. 우선 ```Arduino``` 헤더 파일을 불러와줍니다. 그리고 ```Serial``` 객체를 생성하여 표준 에러를 출력할 준비를 합니다. 다음으로, ```Serial.print()``` 메소드를 사용하여 에러 메시지를 출력할 수 있습니다. 마지막으로 ```.println()``` 메소드를 사용하여 출력을 마칩니다.

```Arduino
#include <Arduino.h>

void setup() {
  Serial.begin(9600); // 시리얼 통신 초기화
}

void loop() {
  // 프로그램 오류 발생
  int x = 1 / 0;
  
  // 표준 에러로 출력
  Serial.print("Error occurred: ");
  Serial.println("Division by zero"); 
}
```

위의 코드를 실행하면, 시리얼 모니터에 "Error occurred: Division by zero"라는 에러 메시지가 출력됩니다.

## Deep Dive

표준 에러는 프로그램에서 발생하는 모든 에러를 저장하는 버퍼입니다. 따라서, 하나의 에러 메시지만 출력되는 것이 아니라 여러 개의 에러 메시지를 출력할 수 있습니다. 이는 디버깅 과정에서 유용하게 사용될 수 있습니다. 또한, 표준 에러는 표준 출력과는 별도의 공간을 사용하기 때문에, 프로그램의 정상적인 출력을 방해하지 않으면서 에러 메시지를 출력할 수 있습니다.

## See Also

- [Serial.println()](https://www.arduino.cc/reference/en/language/functions/communication/serial/println/)
- [Debugging with Serial Monitor](https://www.arduino.cc/en/Guide/Introduction#toc11)