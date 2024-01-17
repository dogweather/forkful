---
title:                "랜덤 숫자 생성하기"
html_title:           "Arduino: 랜덤 숫자 생성하기"
simple_title:         "랜덤 숫자 생성하기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
랜덤 숫자 생성이란 무엇인가? 프로그래머들이 왜 이것을 하는 걸까?
"랜덤 숫자 생성이란 컴퓨터 프로그램에서 무작위로 숫자를 생성하는 것을 의미합니다. 이를 통해 다양한 상황에서 유용한 무작위 값을 얻을 수 있습니다. 예를 들어 게임에서 랜덤으로 맵이 생성되거나 암호화에 사용되는 비밀키를 생성할 때 랜덤 숫자 생성이 필요합니다."

## 어떻게:
```
Arduino에서 랜덤 숫자를 생성하는 방법은 매우 간단합니다. random() 함수를 사용하면 됩니다. 이 함수는 항상 0에서 32767 사이의 숫자를 반환하며, 이 숫자를 원하는 범위로 조절할 수 있습니다.
```
```
#include <stdlib.h>

int randomNumber = random(0, 100); // 0에서 100 사이의 랜덤 숫자 생성
Serial.println(randomNumber); // 생성된 숫자를 시리얼 모니터에 출력
```
```
출력:
57
```

## 깊이있게:
"컴퓨터에서 랜덤 숫자를 생성하는 과정은 실제로 완벽한 무작위를 보장하지 않습니다. 그렇기 때문에 좀 더 안전한 암호화를 위해서는 다른 무작위 생성 방식이 필요할 수 있습니다. 예를 들어 'entropy'라는 개념을 사용하여 컴퓨터의 여러 요소들을 조합해 더 높은 무작위성을 보장하는 방식이 있습니다."

"Arduino의 random() 함수는 유사 무작위 생성기를 사용하여 랜덤 숫자를 생성합니다. 이는 무작위성을 보장하지는 않지만 많은 경우에는 충분히 사용할 수 있습니다. 만약 더 안전하고 높은 무작위성이 필요하다면 외부 무작위 생성기 모듈을 이용하거나 더 복잡한 알고리즘을 사용하는 것도 방법입니다."

## 더 알아보기:
- [Arduino 공식 랜덤 함수 문서](https://www.arduino.cc/reference/ko/language/functions/random-numbers/random/)
- [Entropy 개념 자세히 알아보기](https://en.wikipedia.org/wiki/Entropy_(computing))
- [외부 무작위 생성기 모듈 추가하기](https://www.arduino.cc/en/Tutorial/AddicoreRandomNumber)