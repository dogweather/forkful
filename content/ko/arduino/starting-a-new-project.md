---
title:    "Arduino: 새로운 프로젝트 시작하기"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/starting-a-new-project.md"
---

{{< edit_this_page >}}

### 왜
새로운 프로젝트를 시작하는 이유는 여러 가지가 있습니다. 아두이노는 컴퓨터이지만 코딩이 쉽고 저렴하기 때문에 많은 사람들이 처음으로 프로그래밍을 시작하는 도구로 사용하고 있습니다.

### 사용 방법
아두이노 프로그래밍을 시작하는 가장 기본적인 방법은 먼저 아두이노 IDE를 설치하는 것입니다. 이후에는 간단한 하드웨어 연결과 코드 작성을 통해 첫 프로젝트를 만들 수 있습니다. 아래는 LED를 제어하는 예제 코드입니다.

```Arduino
int LED = 13; // LED가 연결된 핀 번호 설정

void setup() {
    pinMode(LED, OUTPUT); // LED 핀을 출력으로 설정
}

void loop() {
    digitalWrite(LED, HIGH); // LED 켜기
    delay(1000); // 1초 기다리기
    digitalWrite(LED, LOW); // LED 끄기
    delay(1000); // 1초 기다리기
}
```

위 코드를 실행하면 LED가 1초 간격으로 켜졌다가 꺼지는 것을 볼 수 있습니다.

### 심층 분석
새로운 아두이노 프로젝트를 시작할 때 가장 중요한 것은 문제 해결 능력입니다. 어떤 문제를 해결하고 싶은지 명확하게 정의하고, 그에 맞는 하드웨어와 적절한 코드를 선택해야 합니다. 또한 잘 정리된 코드를 작성하는 것도 중요합니다. 긴 프로그램을 작성할 경우 적절한 주석을 달고 코드를 함수로 나누는 것이 좋습니다. 이를 통해 코드를 이해하기 쉽고 유지보수하기 쉽게 할 수 있습니다.

### 더 알아보기
아두이노 프로그래밍을 배우는 가장 좋은 방법은 예제 코드를 따라해 보고 이해하는 것입니다. 아래 링크를 통해 다양한 예제 코드를 확인하고 프로젝트에 응용해 보세요.

- [아두이노 공식 홈페이지](https://www.arduino.cc/)
- [아두이노 공식 예제 코드](https://www.arduino.cc/en/Tutorial/HomePage)
- [마이크로소프트 메이크코드 아두이노 확장팩](https://makercurrent.com/2446)
- [실시간 아두이노 온라인 시뮬레이터](https://www.tinkercad.com/)

### 참고 자료
- [아두이노 프로그래밍 강좌](https://www.youtube.com/playlist?list=PLSvCYhYoKAn7WYc0xHJKastXUcxIZaObQ)
- [아두이노 프로그래밍 가이드](https://www.makerlab.or.kr/?p=10174)