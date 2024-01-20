---
title:                "임의의 숫자 생성하기"
html_title:           "Elixir: 임의의 숫자 생성하기"
simple_title:         "임의의 숫자 생성하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 그렇게 합니까?

랜덤 숫자 생성은 일련의 숫자를 예측할 수 없는 방식으로 생성하는 프로세스입니다. 프로그래머들은 게임, 임의의 이벤트 시뮬레이션, 보안 암호화 등에서 랜덤성을 달성하기 위해 이것을 사용합니다.

## 어떻게:

```Gleam
import gleam/list.{shuffle}
import gleam/int
import gleam/random.{default}

fn main() {
  let nums = list.range(0, 10)
  let seed = int.from_float(time.now().nanosecond())
  let generator = default.generator(seed)
  
  nums
  |> shuffle(generator)
  |> list.to_string
  |> io.println
}
```
위의 코드의 예시 결과는 임의의 숫자 배열을 출력합니다, `"[8, 3, 5, 2, 9, 1, 0, 7, 4, 6]"`.

## 깊게 들어가기:

랜덤 숫자 생성은 고대 그리스 시대부터 수학자들과 철학자들이 적용하고 탐구해 온 분야입니다. 컴퓨팅의 초기 단계에서 이 개념은 알고리즘이 예측할 수 없는 숫자를 생성할 수 있도록 설계되어 컴퓨터 사이언스의 핵심 부분이 되었습니다.

Gleam에서도 `gleam/random` 모듈의 `generator` 함수를 이용해 난수를 생성할 수 있습니다. 이 때, `gleam/int` 모듈의 `from_float` 함수를 사용하여 시드값을 설정합니다.

랜덤 숫자 생성에 대한 대안으로는 의사-난수 생성기(Pseudo-Random Number Generator, PRNG)가 있습니다. 이는 숫자의 순서를 만들어내지만, 충분한 계산량이 주어지면 그 패턴을 예측할 수 있습니다.

## 참고하십시오:

3. [PRNG의 상세 내용](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)