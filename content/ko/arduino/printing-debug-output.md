---
title:                "Arduino: 디버그 출력 출력"
programming_language: "Arduino"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

# 왜

디버그 메시지를 출력하는 것의 이유는 프로그래밍 중에 오류를 추적하고 해결하는 데에 큰 도움이 됩니다. 디버그 출력을 사용하면 프로그램이 실행되는 동안 어떤 값이나 변수가 변하는지, 어떤 함수나 루프가 정확하게 작동하는지 확인할 수 있습니다.

# 방법

만약 우리가 `pinMode()` 함수로 아두이노 핀을 설정해야 한다고 가정해 봅시다. 예를 들어, 우리가 13번 핀을 출력으로 설정하고 싶은 경우 다음과 같이 코드를 작성할 수 있습니다.

```Arduino
pinMode(13, OUTPUT);

```

그러나 우리가 실제로 핀의 상태를 확인하고 싶다면, 다음과 같이 코드에 디버그 출력을 추가할 수 있습니다.

```Arduino
pinMode(13, OUTPUT);
Serial.println(digitalRead(13)); //디버그 출력
```

이렇게 하면 프로그램 실행 중에 핀의 상태를 확인할 수 있습니다. 만약 핀을 입력으로 설정하고서도 디버그 출력을 사용하고 싶다면, 다음과 같이 `INPUT` 매개변수를 사용하면 됩니다.

```Arduino
pinMode(13, INPUT);
Serial.println(digitalRead(13)); //디버그 출력
```

# 깊게 파헤치기

디버그 출력은 프로그래밍 중에 오류를 추적할 때 매우 유용하지만, 프로그램이 끝난 후 디버그 출력을 제거하는 것을 잊지 마세요. 디버그 출력은 프로그램의 메모리를 소모하므로, 프로그램을 완성한 후에는 꼭 지워주어야 합니다.

또한, 디버그 출력을 효과적으로 사용하기 위해서는 적절한 메시지를 출력해야 합니다. 메시지는 간결하면서도 프로그램의 실행 상태를 명확하게 설명할 수 있도록 해야 합니다.

# 관련 링크

- [아두이노 공식 홈페이지](https://www.arduino.cc/)
- [아두이노 포럼](https://forum.arduino.cc/)
- [아두이노 디버깅 가이드](https://learn.sparkfun.com/tutorials/how-to-debug-your-arduino-project)