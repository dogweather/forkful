---
title:                "Arduino: 표준 에러로 쓰기"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# 왜
왜 standard error에 대한 쓰기를 할까요? 실수를 찾는데 도움이 되기 때문입니다.

# 호우투
standard error에 대한 쓰기는 간단합니다. 다음의 코드 블록을 참고하시기 바랍니다.
```Arduino
// 예제 코드
int num = 10;
Serial.println("Standard error: " + num);

// 예상 출력 결과:
Standard error: 10
```

# 딥 다이브
standard error에 대해 알아야 할 더 깊은 내용이 있습니다. 우선, standard error는 프로그램에서 발생하는 에러 메시지를 의미합니다. 이 에러 메시지는 프로그램이 실행되는 동안 발생하는 예기치 않은 상황을 나타내며, 이를 통해 버그를 찾거나 프로그램을 디버깅할 수 있습니다.

한 가지 중요한 차이점은 standard error가 단순한 출력 문장이 아니라는 것입니다. standard error는 표준 출력과 달리 버퍼링되지 않기 때문에, 프로그램이 종료되지 않으면 바로 출력됩니다. 이로 인해, 프로그램이 비정상적으로 종료되거나 강제 종료되더라도 standard error 메시지는 출력됩니다. 따라서 프로그램을 디버깅할 때 매우 유용합니다.

# 참고 자료
- [Arduino Serial.println() - Arduino Reference](https://www.arduino.cc/reference/en/language/functions/communication/serial/println/)
- [Standard Error - GeeksforGeeks](https://www.geeksforgeeks.org/standard-error/)
- [Standard Error vs Standard Output - Linux Hint](https://linuxhint.com/standard-error-vs-standard-output/)