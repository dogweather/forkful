---
title:                "Arduino: 랜덤 숫자 생성"
programming_language: "Arduino"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

만약 당신이 아두이노 프로그래밍을 재미있게 하고 싶거나, 다양한 프로젝트를 시도하고 싶다면, 랜덤한 숫자를 생성하는 것은 흥미로운 방법입니다. 지금부터 우리는 왜 랜덤한 숫자를 생성하는지에 대해 알아보고, 그것을 어떻게 할 수 있는지 살펴보겠습니다. 그리고보다 깊이있는 정보를 알아보도록하겠습니다.

## 왜

컴퓨터 과학 분야에서 랜덤한 숫자는 매우 중요합니다. 예를 들어, 암호화, 검증, 무작위 관련 테스트 등 다양한 분야에서 우리는 랜덤한 숫자가 필요합니다. 랜덤한 숫자를 생성함으로서 우리는 예측할 수 없는 결과를 얻을 수 있고, 더 나은 보안을 제공할 수 있습니다.

## 어떻게

우리는 아두이노의 랜덤함수를 사용하여 랜덤한 숫자를 생성할 수 있습니다. 아래코드에서 우리는 "random"함수를 사용하여 0~255 사이의 숫자를 출력하는 예제입니다.

```Arduino

void setup() {
  //시리얼 모니터 사용
  Serial.begin(9600);
}

void loop() {
  //랜덤한 숫자 생성
  int num = random(256);
  //숫자 출력
  Serial.println(num);
  delay(1000); //1초 대기
}

```

위와 같이 코드를 작성하고 아두이노를 실행시키면, 매초마다 0~255 사이의 랜덤한 숫자가 출력될 것입니다. 이것은 아주 간단한 예제이며, 더 복잡한 로직을 추가하여 더 다양한 형태의 랜덤한 숫자를 출력할 수 있습니다.

## 딥 다이브

위 예제에서는 언급되지 않았지만, 랜덤한 숫자를 생성하는 데는 다양한 알고리즘과 방법론이 있습니다. 이러한 알고리즘들은 더 큰 범위의 랜덤한 숫자를 생성하는 데 사용될 수 있습니다. 일반적으로 랜덤한 숫자를 생성하기 위해서는 컴퓨터 시스템에서 일어나는 다양한 물리적 이벤트들을 이용합니다.

## 더 알아보기

- [랜덤함수에 대한 아두이노 공식 문서](https://www.arduino.cc/reference/ko/language/functions/random-numbers/random/)
- [랜덤숫자 생성의 더 복잡한 방법 (영문)](https://stackoverflow.com/questions/48053/what-is-the-best-way-to-generate-a-random-key-within-a-range-in-as3)