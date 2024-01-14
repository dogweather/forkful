---
title:    "Gleam: 랜덤 숫자 생성"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 왜:

컴퓨터 프로그래밍에서 무작위 숫자를 생성하는 것은 매우 일반적입니다. 무작위 숫자는 게임을 만드는 데 사용되며, 암호 생성기 및 분포 시계열과 같은 여러 응용 프로그램에서 중요한 역할을 합니다. 이 포스트에서는 Gleam에서 무작위 숫자를 생성하는 방법을 살펴보겠습니다.

## 어떻게:

"```Gleam
import gleam/random

let num = random.int(1, 100)

io.println("Random number between 1 and 100: " ++ to_string(num))
```"

위의 코드는 1에서 100 사이의 무작위 정수를 생성하고 출력하는 간단한 예제입니다. 이 코드를 실행하면 다음과 같은 출력이 나옵니다:

Random number between 1 and 100: 57

Gleam에서 무작위 숫자를 생성하는 가장 기본적인 방법은 random 모듈을 사용하는 것입니다. random 모듈에는 다양한 함수가 있으며, 이 함수들은 다음과 같은 서명을 가지고 있습니다:

- bool: 그것은 그냥 참인지, 그냥 거짓인지 나타내는 무작위 부울 값을 반환합니다.
- float(min: Float, max: Float): 최소 및 최대 범위 내에서 무작위 부동 소수점 값을 반환합니다.
- int(min: Int, max: Int): 최소 및 최대 범위 내에서 무작위 정수 값을 반환합니다.
- int_inclusive(min: Int, max: Int): 최소 및 최대 범위 내에서 무작위 정수 값을 반환합니다. 단, 이 함수는 최대 값을 포함합니다.

다양한 목적에 맞게 이러한 함수를 사용할 수 있으며, 무작위 숫자를 생성하는 데 필요한 규칙을 쉽게 지정할 수 있습니다.

## 깊은 다이브:

무작위 숫자가 생성되는 방식에는 여러 가지가 있습니다. 하나의 방법은 난수 발생기(Random Number Generator, RNG)를 사용하는 것입니다. RNG는 무작위성을 기반으로 숫자를 생성하는 알고리즘입니다. 이 알고리즘은 시작 시드(initial seed) 값을 사용하여 무작위성을 쉽게 조작할 수 있습니다. 이를 통해 동일한 시작 시드 값을 사용하면 항상 동일한 무작위 숫자를 얻을 수 있습니다.

또 다른 방법은 무작위성이 대기 시간, 마우스의 움직임, 네트워크 패킷 등과 같은 실제 환경에서 발생하는 외부 요소에 의해 결정되는 것입니다. 이를 의사 난수(Pseudo-Random Number)라고 부릅니다. 의사 난수는 완벽한 무작위성은 아니지만, 일반적으로 충분한 무작위성을 제공합니다.

Gleam에서는 난수 발생기를 사용하여 무작위성을 조작할 수 있습니다. 이를 통해 다양한 응용 프로그램에서 필요한 수준의 무작위성을 제공할 수 있습니다.

## 참고:

- [Gleam 공식 문서](https://gleam.run/documentation/index)
- [Gleam 무작위 모듈의 소스 코드](https://github.com/gleam-lang/gleam_stdlib/blob/master/random/src/random.gleam)