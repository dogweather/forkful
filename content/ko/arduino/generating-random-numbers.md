---
title:                "Reply with 난수 생성난수 생성"
html_title:           "Arduino: Reply with 난수 생성난수 생성"
simple_title:         "Reply with 난수 생성난수 생성"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 왜?

작은 확률의 가능성을 고려해야 할 때, 또는 어떤 결정을 무작위로 내리려고 할 때 무작위 수를 생성할 수 있습니다. 그리고 그렇게 할 수 있도록 아두이노는 난수(random number) 기능을 제공합니다.

## 활용 방법

```Arduino
void setup() {
  Serial.begin(9600); // 컴퓨터와 시리얼 통신을 시작합니다.
  randomSeed(analogRead(A0)); // 아날로그 핀 A0를 이용해 발생한 값으로 랜덤 시드를 설정합니다.
}

void loop() {
  int randomNumber1 = random(0, 10); // 0부터 10 사이의 정수 랜덤 수를 생성합니다.
  float randomNumber2 = random(0.0, 1.0); // 0부터 1 사이의 실수 랜덤 수를 생성합니다.
  Serial.println(randomNumber1); // 생성한 랜덤 수를 시리얼 모니터로 출력합니다.
  Serial.println(randomNumber2);
  delay(1000); // 1초마다 랜덤 수를 생성합니다.
}
```

위의 코드는 다양한 데이터 유형의 난수를 생성하는 방법을 보여줍니다. 컴퓨터와 아두이노를 시리얼 통신을 통해 연결하고, `randomSeed()` 함수를 사용하여 시드 값을 설정한 후 `random()` 함수를 이용해 랜덤 수를 생성합니다. 생성한 랜덤 수는 `Serial.println()` 함수를 사용하여 시리얼 모니터로 출력됩니다. `delay()` 함수를 통해 1초마다 무한히 랜덤 수를 생성하도록 설정하였습니다.

## 깊이 파헤치기

아두이노에서 제공하는 난수 생성 함수는 `random()` 뿐만 아니라 `randomSeed()`와 `randomBytes()`도 있습니다. `random()` 함수는 사용자가 지정한 범위 내에서 랜덤 값을 생성하는 데 사용되고, `randomSeed()` 함수는 시드 값을 설정하여 랜덤 패턴을 제어할 수 있습니다.

또한 `randomBytes()` 함수를 이용하면 여러 개의 난수를 한 번에 생성할 수 있습니다. 그리고 이를 이용하여 보안 관련 프로그램 등에 활용할 수 있습니다.

```
https://www.arduino.cc/reference/en/language/functions/random-numbers/randombytes/
https://www.arduino.cc/reference/en/language/functions/random-numbers/randomseed/
```

## 관련 항목

```
https://www.arduino.cc/en/Tutorial/RandomLetters
https://www.arduino.cc/en/Tutorial/RandomMines
```