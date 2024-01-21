---
title:                "난수 생성하기"
date:                  2024-01-20T17:49:54.040600-07:00
model:                 gpt-4-1106-preview
simple_title:         "난수 생성하기"
programming_language: "Lua"
category:             "Lua"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 무엇인가 & 왜 사용하는가?
랜덤 숫자 생성은 예측 불가능한 숫자를 만드는 과정입니다. 프로그래머들은 게임, 시뮬레이션, 보안에서 무작위 요소가 필요할 때 이를 사용합니다.

## 사용 방법:
Lua에서 랜덤 숫자를 생성하기 위해 `math.random` 함수를 사용합니다. 먼저 `math.randomseed()`로 시드를 설정하세요.

```Lua
math.randomseed(os.time())  -- 시드 설정

print(math.random())        -- 0과 1 사이 랜덤 값 반환
print(math.random(5))       -- 1부터 5 사이 랜덤 정수 반환
print(math.random(10, 20))  -- 10부터 20 사이 랜덤 정수 반환
```

위 코드를 실행하면 다음과 같은 결과가 출력됩니다 (결과는 매번 달라집니다):

```
0.0012512588885157
3
17
```

## 심화 학습:
랜덤 숫자를 만드는 것이 마법처럼 보일 수도 있지만, 대부분의 랜덤 함수는 실제로 `의사 난수 생성기(Pseudo-random number generator, PRNG)`를 사용합니다. 이는 복잡한 알고리즘을 통해 순차적으로 나오는 숫자들이 무작위인 것처럼 보이게 만들지만, 시작점(시드)만 같다면 같은 수열을 생성합니다.

Lua는 일반적으로 C 라이브러리의 PRNG를 사용합니다. `math.randomseed`를 호출하지 않으면, 프로그램은 같은 시드를 가지고 시작하여 같은 랜덤 시퀀스를 반복합니다.

다른 언어나 라이브러리에서는 다양한 PRNG 알고리즘이 제공됩니다. 예를 들어, Python은 `random` 모듈을 사용하고 C++에서는 `<random>` 헤더를 통해 더 복잡한 엔진과 분포를 제공합니다.

보안이 중요한 목적을 위해서는 보통의 PRNG 대신 암호학적으로 안전한 난수 생성기(Cryptographically Secure Pseudorandom Number Generator, CSPRNG)를 사용해야 합니다.

## 관련 자료:
- Lua 공식 문서: https://www.lua.org/manual/5.4/
- 이론적 배경에 대한 더 많은 정보: https://en.wikipedia.org/wiki/Pseudorandom_number_generator
- CSPRNG에 대한 정보: https://en.wikipedia.org/wiki/Cryptographically_secure_pseudorandom_number_generator