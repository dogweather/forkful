---
title:                "랜덤 숫자 생성하기"
html_title:           "Lua: 랜덤 숫자 생성하기"
simple_title:         "랜덤 숫자 생성하기"
programming_language: "Lua"
category:             "Lua"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
난수를 생성한다는 것은 프로그래머가 무작위로 숫자를 생성하는 것을 의미합니다. 이것은 암호학, 게임, 통계 등 다양한 분야에서 사용됩니다.

## 하는 법:
```Lua
-- 1부터 10까지 무작위 숫자 생성
print(math.random(1, 10))

-- 랜덤 실수 생성
print(math.random())

-- 난수 시드 설정
math.randomseed(os.time())

-- 랜덤 문자열 생성
local letters = {'a', 'b', 'c'}
local randomLetter = letters[math.random(1, #letters)]
print(randomLetter)
```

위 코드를 실행하면 다음과 같은 결과가 출력됩니다:

```Lua
8
0.874392094
b
```

## 자세히 알아보기:
1. Lua에서는 `math.random()` 함수를 사용하여 난수를 생성할 수 있습니다.
2. 시드(seed)를 설정하여 더 다양한 난수를 생성할 수 있습니다.
3. 만약 시드를 설정하지 않는다면, 시스템 시간을 기반으로 자동으로 설정됩니다.

## 더 알아보기:
- [Lua 공식 문서 math.random()](https://www.lua.org/manual/5.4/manual.html#pdf-math.random)
- [난수 생성의 역사](https://en.wikipedia.org/wiki/Random_number_generation)
- [난수 생성에 대한 다른 방법들](https://www.makeuseof.com/tag/random-number-generators-explained/)

라이선스:
이 글은 MIT License를 따릅니다. 누구나 자유롭게 사용할 수 있으며, 원하는 대로 수정할 수 있습니다.