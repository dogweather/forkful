---
title:                "Arduino: 테스트 작성하기"
programming_language: "Arduino"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/writing-tests.md"
---

{{< edit_this_page >}}

# 왜 테스트를 작성해야 하는가

아두이노 프로그래밍은 매우 재미있고, 흥미로우며, 미래를 위한 많은 가능성을 제공합니다. 그러나, 가끔은 우리가 작성한 코드가 제대로 작동하지 않는 경우도 있습니다. 이때 테스트를 작성하면 코드의 오류를 식별하고 해결할 수 있습니다.

## 작성하는 방법 

작성하려는 테스트에 대한 간단한 예제를 만들어 보겠습니다. 우리의 목표는 LED를 제어하는 코드를 작성하는 것입니다.

```Arduino
int LED = 13;
void setup(){ 
    pinMode(LED, OUTPUT);
}

void loop(){ 
    digitalWrite(LED, HIGH);
    delay(1000);
    digitalWrite(LED, LOW);
    delay(1000);
}
```

이 예제에서는 LED를 1초씩 켜고 끈 후, 다시 켜고 끈 후, 무한히 반복하는 코드입니다.

이제 테스트를 작성해 보겠습니다.

```Arduino
int LED = 13;
int delayTime = 1000;

void setup(){ 
    pinMode(LED, OUTPUT);
}

void loop(){ 
    digitalWrite(LED, HIGH);
    delay(delayTime);
    digitalWrite(LED, LOW);
    delay(delayTime);
}
```

위의 테스트는 이전 예제와 거의 동일하지만, 변수 delayTime을 추가했습니다. 이제 우리는 delayTime 변수를 조정하여 LED의 켜고 끄는 시간을 변경할 수 있습니다.

## 깊이 파고들기

테스트는 코드의 오류를 찾아내는 것 외에도 실제로 코드를 작성하는데 도움이 됩니다. 테스트를 작성하면 코드를 작성하는 과정에서 오류를 미리 방지할 수 있습니다. 또한, 테스트 코드를 작성하면 기존 코드를 변경해도 코드의 오리지널 기능을 유지할 수 있습니다.

예를 들어, 위의 예제에서 우리는 LED의 켜고 끄는 시간을 변경할 수 있게 변수를 추가했습니다. 만약 이 코드가 다른 코드와 결합되어 작동할때, 우리는 delayTime 변수를 변경함으로써 전체 시스템의 행동을 바꿀 수 있습니다. 이렇게 하면 불필요한 코드를 추가하지 않고도 코드를 변경할 수 있습니다.

## 더 찾아보기

더 자세한 정보를 얻고 싶다면 아래의 링크를 참조해 보세요.

[아두이노 테스트 코드 작성 방법](https://www.arduino.cc/en/Guide/ArduinoUnitTesting)

[아두이노에 대해 더 알아보기](https://www.arduino.cc/)

# 관련 링크

[아두이노 공식 홈페이지](https://www.arduino.cc/)

[아두이노 코딩 관련 팁](https://www.arduino.cc/en/Tutorial/HomePage)

[아두이노 커뮤니티에서 다른 개발자들과 의견 공유하기](https://www.arduino.cc/en/Community)