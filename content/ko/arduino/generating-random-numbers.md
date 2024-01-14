---
title:                "Arduino: 임의의 숫자 생성"
simple_title:         "임의의 숫자 생성"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

# 왜
아두이노 프로그래밍을 할 때 랜덤 숫자를 생성하는 이유는 다양합니다. 예를 들어 게임에서 랜덤한 이벤트가 발생할 때 사용하거나, 센서의 값이 랜덤하게 변하는 경우에 사용할 수 있습니다. 또는 무작위로 선택할 필요가 있는 경우 등 다양한 상황에서 랜덤 숫자를 생성하는 것이 유용합니다.

## 어떻게
아래의 코드 블록을 통해 아두이노에서 랜덤 숫자를 생성하는 방법을 알아보겠습니다. 먼저, random() 함수를 사용하여 0부터 255 사이의 랜덤한 숫자를 생성합니다. 이어서, generatedNumber 변수에 저장하고, serial monitor를 통해 값을 확인할 수 있습니다. 또한, random(min, max) 함수를 사용하여 범위를 지정할 수도 있습니다. 아래의 코드에서는 10부터 50 사이의 랜덤한 숫자를 생성하고, generatedNumber 변수에 저장하여 serial monitor에서 출력하는 예시를 보여줍니다.

```Arduino
// 예시 1: 0부터 255 사이의 랜덤한 숫자 생성하기
int generatedNumber = random(255); // 랜덤 숫자를 generatedNumber 변수에 저장
Serial.print("Generated number: "); // serial monitor에 출력할 문구
Serial.println(generatedNumber); // generatedNumber 변수의 값 출력

// 예시 2: 10부터 50 사이의 랜덤한 숫자 생성하기
int generatedNumber = random(10, 50); // 10부터 50 사이의 랜덤 숫자를 generatedNumber 변수에 저장
Serial.print("Generated number: "); // serial monitor에 출력할 문구
Serial.println(generatedNumber); // generatedNumber 변수의 값 출력
```

**출력:**

```Arduino
Generated number: 35
```

## 딥 다이브
아두이노에서 랜덤 숫자를 생성하기 위해서는 randomSeed() 함수가 필요합니다. 이 함수는 랜덤 숫자를 생성할 때 기준이 되는 시드값을 설정합니다. 시드값은 매우 중요한 요소로, 같은 시드값을 이용하면 같은 순서의 랜덤 숫자를 생성할 수 있습니다. 따라서, 시드값을 변경하여 다양한 랜덤한 숫자를 생성할 수 있습니다.

아래의 코드는 시간 값을 이용하여 시드값을 설정하는 예제입니다. 시간 값은 항상 변하는 값이기 때문에, 매번 다른 시드값이 생성되어 더 다양한 랜덤 숫자를 생성할 수 있습니다.

```Arduino
// 예시: 시간 값을 이용하여 시드값 설정하기
unsigned long currentTime = millis(); // millis() 함수를 사용하여 현재 시간 값을 받아옴
randomSeed(currentTime); // currentTime 값으로 시드값 설정
```

## 씨 알소
랜덤 숫자를 생성하는데 사용할 수 있는 다양한 함수와 방법이 있지만, 보다 상세한 지식을 알고 싶다면 씨 알소(CSAIL)의 유용한 논문을 참고해보세요. 이 논문에서는 아두이노에서 사용하는 랜덤 함수의 동작 원리와 더 좋은 랜덤 숫자를 생성하기 위한 방법 등을 소개해줍니다.

# 봐도 좋아
- [아두이노 랜덤 함수 공식 문서](https://www.arduino.cc/reference/en/language/functions/random-numbers/random/)
- [씨 알소(CSAIL) 논문: Generating Random Numbers Based on User Input](https://dspace.mit.edu/bitstream/handle/1721.1/36323/57551907