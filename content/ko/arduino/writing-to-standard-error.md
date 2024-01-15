---
title:                "표준 에러에 쓰는 방법"
html_title:           "Arduino: 표준 에러에 쓰는 방법"
simple_title:         "표준 에러에 쓰는 방법"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 왜?

프로그래밍을 할 때 많은 사람들이 표준 오류를 사용하는 이유는 작성한 코드의 디버깅에 도움이 되기 때문입니다. 또한, 오류가 발생할 때 실시간으로 즉각적인 알림을 받아 수정할 수 있게 해줍니다. 

## 어떻게?

아래는 Arduino에서 표준 오류를 사용하는 예시 코드입니다.

```
Arduino.println("Error: Cannot connect to WiFi!");
```

실행하면 다음과 같은 출력 결과를 볼 수 있습니다.

```
Error: Cannot connect to WiFi!
```

위와 같이 코드에 예외처리를 넣고 싶은 부분에 `Arduino.println()`을 사용해서 출력할 수 있습니다.

## 깊이 파고들어보기

보다 정확하게 오류를 파악하고 디버깅하기 위해서는 표준 오류 대신 `Serial.println()`을 사용하는 것이 좋습니다. 이 함수는 변수의 값을 출력해줄 수 있고, `Serial Monitor`라는 도구를 사용하면 시리얼 모니터를 통해 변수의 값을 확인할 수 있습니다. 또한, `Serial.print()`와 `Serial.write()`를 사용하면 고급 디버깅에 더 유용한 정보를 얻을 수 있습니다.

## 더 알아보기

- Arduino 공식 문서: https://www.arduino.cc/reference/en/language/functions/communication/serial/println/
- 팀 타이머: https://techtimer.io/tutorials/serial-beginner
- 아두이노를 활용한 IoT 개발: https://www.hackster.io/ertd/microcontroller-web-server-with-a-rasberi-piapis-empl-resource-copy-ba7c21