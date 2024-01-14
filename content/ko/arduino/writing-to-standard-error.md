---
title:    "Arduino: 표준 오류에 작성하기"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 왜

왜 누군가가 표준 오류에 쓰는 것을 시작하게 될까요? 아두이노 프로그래밍을 할 때 여러분은 종종 조치를 취하기 전에 디바이스가 무엇을 하고 있는지 알고 싶을 때가 있을 겁니다. 이것이 바로 표준 오류를 쓰는 이유입니다. 표준 오류는 디바이스의 상태나 에러 메시지 등을 보여줌으로써 여러분이 프로그램을 좀 더 안전하고 이해하기 쉽게 만들어줍니다.

## 어떻게

아두이노에서 표준 오류를 쓰는 것은 아주 간단합니다. 여러분이 할 일은 `Serial.println()`을 사용해서 내용을 보내는 것뿐입니다. 예를 들어, 디바이스의 온도를 읽어와서 표준 오류로 출력하는 코드는 다음과 같습니다.

```Arduino
int temp = analogRead(A0); // A0는 핀 번호
Serial.println(temp);
```

위의 코드는 디바이스의 온도를 읽어온 후, 표준 오류에 출력합니다. 여러분은 이 코드를 다른 함수나 조건문 등에도 적용할 수 있습니다. 이렇게 하면 여러분이 프로그램을 디버깅할 때 디바이스의 상태를 보다 쉽게 확인할 수 있습니다.

## 깊이 파고들기

보다 깊이 들어가서 설명하면, 표준 오류는 프로그램 실행 중에 어떤 문제가 발생했는지 여러분이 파악하고 해결할 수 있도록 도와줍니다. 만약 여러분이 로그 파일을 사용한다면, 표준 오류 메시지를 저장하고 나중에 분석할 수도 있을 것입니다. 또한 디바이스에 문제가 발생했을 때 주변에 있는 사람들에게도 도움을 줄 수 있습니다. 디바이스에 연결된 PC에서 실시간으로 표준 오류를 모니터링하면서 문제가 발생했을 때 즉시 조치를 취할 수 있습니다.

## 이것도 참고해보세요

- [아두이노 공식 홈페이지](https://www.arduino.cc/)
- [아두이노 표준 라이브러리](https://www.arduino.cc/reference/en/libraries/)
- [Serial.println() 설명서](https://www.arduino.cc/reference/en/language/functions/communication/serial/println/)