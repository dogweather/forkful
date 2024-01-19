---
title:                "임의의 숫자 생성하기"
html_title:           "Elixir: 임의의 숫자 생성하기"
simple_title:         "임의의 숫자 생성하기"
programming_language: "Lua"
category:             "Lua"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

랜덤 번호 생성이란 예측할 수 없는 일련의 숫자를 만드는 행위입니다. 이는 게임, 시뮬레이션, 암호화 등에서 중요한 역할을 합니다.

## 사용법:

Lua에서 랜덤 숫자를 생성하는 것은 간단합니다. `math.random` 함수를 사용하면 됩니다. 아래는 사용 예제입니다.

```Lua
math.randomseed(os.time())

random_number = math.random()
print(random_number)

random_range_number = math.random(1, 100)
print(random_range_number)
```

이 코드는 먼저 `math.randomseed`를 사용하여 초단위의 현재 시간으로 랜덤 발생기를 초기화합니다. 그다음 `math.random`를 호출하여 0과 1 사이의 랜덤한 수를 만들거나, 특정 범위 (예시에서는 1에서 100)에서 랜덤한 정수를 생성합니다.

## 깊이 잠수

Lua에서 `math.random` 함수는 고전적인 `C`의 `rand` 함수에 기반을 두고 있습니다. 그러나 Lua에서는 이 함수를 시드 값으로 초기화 할 수 있는 `math.randomseed`와 결합해 더 유연한 랜덤 숫자 생성을 가능하게 합니다.

대안으로, 보다 복잡한 랜덤 숫자 생성이 필요한 경우, 직접 구현하거나 'luaossl'과 같은 라이브러리를 사용하여 고급 암호화 활용도 가능합니다.

`math.random` 함수는 내부적으로 선형 합동 생성기 (LCG)를 사용하여 랜덤 숫자를 생성합니다. `math.randomseed` 함수는 이 생성기의 시드 값을 설정합니다.

## 참조하기

철저한 이해와 활용을 위해 다음을 참조하십시오:

- Lua의 공식 문서: [math.random](https://www.lua.org/manual/5.3/manual.html#math.random) and [math.randomseed](https://www.lua.org/manual/5.3/manual.html#math.randomseed)
- [Lua-users wiki](http://lua-users.org/wiki/MathLibraryTutorial)