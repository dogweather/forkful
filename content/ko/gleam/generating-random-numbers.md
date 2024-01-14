---
title:                "Gleam: 랜덤 숫자 생성하기"
programming_language: "Gleam"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 왜 랜덤 숫자 생성에 참여해야 할까요?

난수는 컴퓨터 과학 분야에서 중요한 개념입니다. 랜덤 숫자 생성은 시뮬레이션, 보안, 게임 등 다양한 분야에서 필수적으로 사용되는 기술입니다. 이번 블로그 포스트에서는 Gleam 프로그래밍 언어로 랜덤 숫자를 생성하는 방법을 알아보겠습니다.

## 어떻게 하면 될까요?

어떤 프로그래밍 언어를 사용하든지 랜덤 숫자를 생성하는 방법은 거의 비슷합니다. 우선 Gleam의 random 모듈을 import합니다. 그리고 generate 함수를 사용하여 랜덤 숫자를 생성합니다. 아래는 Gleam 코드로 작성된 예시입니다.

```Gleam
import random

random.generate() // 출력 예: 0.564738291
```

위 예시는 0에서 1 사이의 랜덤한 소수를 생성합니다. 만약 0에서 10 사이의 정수를 생성하고 싶다면 아래와 같이 코드를 작성할 수 있습니다.

```Gleam
import random

random.generate_int(0, 10) // 출력 예: 7
```

그 외에도, 문자열, 튜플, 리스트 등 다양한 타입의 랜덤 데이터를 생성할 수 있습니다. 자세한 내용은 Gleam 공식 문서를 참고해 주세요.

## 깊게 파고들기

랜덤 숫자를 생성하는 알고리즘에는 여러 가지가 있지만, 가장 대표적인 방법은 난수 발생기를 사용하는 것입니다. 난수 발생기는 무작위로 숫자를 생성하는 장치이며, 시드(seed) 값을 기준으로 난수를 생성합니다. 시드 값이 같으면 같은 순서로 랜덤한 숫자를 생성하므로, 다양한 시드 값을 사용하여 보다 다양한 결과를 얻을 수 있습니다.

## 더 읽어보기

이번 블로그 포스트에서는 Gleam 프로그래밍 언어로 랜덤 숫자를 생성하는 방법을 살펴보았습니다. Gleam 외에도 다양한 프로그래밍 언어에서 랜덤 숫자를 생성하는 방법이 있으니 참고해 보시기 바랍니다.

* [Python에서 랜덤 숫자 생성하기](https://wikidocs.net/3057)
* [Java에서 랜덤 숫자 생성하기](https://www.geeksforgeeks.org/generating-random-numbers-in-java/)
* [C에서 랜덤 숫자 생성하기](https://www.tutorialspoint.com/c_standard_library/c_function_rand.htm)

## 참고

https://gleam.run/modules/random.html