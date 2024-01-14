---
title:                "Arduino: 테스트 작성하기"
simple_title:         "테스트 작성하기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/writing-tests.md"
---

{{< edit_this_page >}}

## 왜합니까?
프로그램을 개발하면서 테스트 코드를 작성하는 것은 매우 중요합니다. 테스트를 통해 개발자는 코드의 버그를 신속하게 발견하고 수정할 수 있습니다. 또한 테스트 코드는 이미 작성한 코드가 예상한 대로 작동하는지 확인하는 방법이기도 합니다.

## 어떻게 하나요?
아두이노 프로그래밍에서 테스트 코드를 작성하는 것은 매우 간단합니다. 다음은 간단한 코드 예제와 출력 예시입니다.

```Arduino
#define LED 13 // LED 핀 번호 설정
 
void setup() {
  pinMode(LED, OUTPUT); // LED 핀을 출력으로 설정
}
 
void loop() {
  digitalWrite(LED, HIGH); // LED 켜기
  delay(500); // 0.5초 대기
  digitalWrite(LED, LOW); // LED 끄기
  delay(500); // 0.5초 대기
}
```

출력:
LED가 0.5초마다 켜지고 꺼지는 것을 볼 수 있습니다.

## 깊이 알아보기
테스트 코드를 작성할 때 주의할 점 몇 가지가 있습니다. 첫째, 테스트는 가능한 모든 시나리오를 커버하도록 작성되어야 합니다. 두번째, "모의 객체(mock objects)"를 사용하여 실제 하드웨어가 아닌 가상 하드웨어를 사용하여 테스트를 실행하는 것이 좋습니다. 이렇게 함으로써 장치에 대한 의존성을 제거할 수 있습니다.

## 참고자료
- [아두이노 테스트 코드 작성하기](https://www.arduino.cc/en/guide/test-code)
- [아두이노 테스트 코드 워크샵 영상](https://www.youtube.com/watch?v=yDHW5mPBK0M)
- [모의 객체란 무엇인가?](https://en.wikipedia.org/wiki/Mock_object)

# 참고자료