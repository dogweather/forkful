---
title:    "Arduino: 난수 생성"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

# 왜
아두이노를 사용하여 무작위 수를 생성하는 것이 유용한 이유는 다양한 프로젝트에서 무작위성이 필요할 때 입니다. 예를 들어, 무작위로 LED를 깜박이는 무드 램프를 만들거나 무작위로 선택된 음악을 재생하는 무작위 음악 재생기를 만들 수 있습니다.

# 어떻게
무작위 수를 생성하는 방법은 여러 가지가 있지만, 가장 간단하고 효과적인 방법은 `random()` 함수를 사용하는 것입니다. 아래는 `random()` 함수를 이용하여 0부터 9까지의 무작위 정수를 생성하는 예제 코드입니다.

```Arduino
void setup() {
  Serial.begin(9600); // 시리얼 통신 설정
  randomSeed(analogRead(0)); // 무작위성을 위한 초기값 설정
}

void loop() {
  int randomNumber = random(10); // 0 ~ 9 사이의 무작위 정수 생성
  Serial.println(randomNumber); // 무작위 수 시리얼 모니터에 출력
  delay(1000); // 1초 딜레이
}
```

위 코드를 아두이노에 업로드하고 시리얼 모니터를 열어보면, 매순간 바뀌는 무작위 수를 볼 수 있습니다.

# 딥 다이브
`random()` 함수는 아두이노 라이브러리에서 제공하는 함수이며, 0부터 지정한 범위의 무작위 정수를 생성합니다. 매우 큰 범위의 무작위 수가 필요하다면 `random()` 함수를 조합하여 사용하거나, 외부 라이브러리를 사용하는 것을 고려할 수 있습니다. 또한 아두이노 보드에는 아날로그 핀을 통해 외부 신호를 입력받아 무작위성을 더욱 증가시킬 수 있는 `randomSeed()` 함수도 존재합니다.

# 같이 보기
- [아두이노 공식 사이트](https://www.arduino.cc/)
- [아두이노 무작위 수 예제 코드](https://www.arduino.cc/reference/en/language/functions/random-numbers/random/)
- [외부 라이브러리인 RNG(랜덤 넘버 생성기) 라이브러리](https://playground.arduino.cc/Code/RNGLibrary/)