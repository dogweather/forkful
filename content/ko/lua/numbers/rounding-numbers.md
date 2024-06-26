---
date: 2024-01-26 03:46:24.040392-07:00
description: "\uBC29\uBC95: Lua\uB294 \uB2E4\uB978 \uC5B8\uC5B4\uB4E4\uACFC \uB2EC\
  \uB9AC \uAE30\uBCF8\uC801\uC73C\uB85C round \uD568\uC218\uB97C \uD3EC\uD568\uD558\
  \uACE0 \uC788\uC9C0 \uC54A\uB2E4. \uC5ED\uC0AC\uC801\uC73C\uB85C, \uC0AC\uC6A9\uC790\
  \uB294 \uC2A4\uC2A4\uB85C \uC791\uC131\uD558\uAC70\uB098 \uC81C3\uC790 \uB77C\uC774\
  \uBE0C\uB7EC\uB9AC\uB97C \uC0AC\uC6A9\uD574\uC57C \uD588\uB2E4. \uC77C\uBC18\uC801\
  \uC778 \uD574\uACB0 \uBC29\uBC95\uC740 \uC22B\uC790\uC758 \uBD80\uD638\uC5D0 \uB530\
  \uB77C 0.5\uB97C \uB354\uD558\uAC70\uB098 \uBE7C\uACE0 \uB098\uC11C `math.floor()`\uC744\
  \ \uC0AC\uC6A9\uD558\uC5EC \uB0B4\uB9BC\uD558\uACE0\u2026"
lastmod: '2024-04-05T21:53:57.099782-06:00'
model: gpt-4-0125-preview
summary: "Lua\uB294 \uB2E4\uB978 \uC5B8\uC5B4\uB4E4\uACFC \uB2EC\uB9AC \uAE30\uBCF8\
  \uC801\uC73C\uB85C round \uD568\uC218\uB97C \uD3EC\uD568\uD558\uACE0 \uC788\uC9C0\
  \ \uC54A\uB2E4."
title: "\uC22B\uC790 \uBC18\uC62C\uB9BC\uD558\uAE30"
weight: 13
---

## 방법:
```lua
-- Lua에서 기본적인 반올림은 내장되어 있지 않으나, 함수를 정의할 수 있다:

function round(num)
    return num >= 0 and math.floor(num + 0.5) or math.ceil(num - 0.5)
end

print(round(3.5))  -- 4
print(round(2.3))  -- 2
print(round(-1.6)) -- -2

-- 특정 소수점 자리로 반올림하기:
function round(num, decimalPlaces)
    local mult = 10^(decimalPlaces or 0)
    return math.floor(num * mult + 0.5) / mult
end

print(round(3.14159, 2)) -- 3.14
print(round(1.98765, 3))  -- 1.988
```

## 심층 분석
Lua는 다른 언어들과 달리 기본적으로 round 함수를 포함하고 있지 않다. 역사적으로, 사용자는 스스로 작성하거나 제3자 라이브러리를 사용해야 했다. 일반적인 해결 방법은 숫자의 부호에 따라 0.5를 더하거나 빼고 나서 `math.floor()`을 사용하여 내림하고 `math.ceil()`을 사용하여 올림하는 것을 의존한다.

자신만의 함수를 만드는 대안으로는 "lua-users wiki" 또는 "Penlight"와 같은 라이브러리들이 있다. 각각은 추가 기능이나 더 큰 오버헤드와 같은 이점과 트레이드오프를 가지고 있다.

내부적으로, 이 함수들은 컴퓨터가 부동 소수점 숫자를 저장하는 방식을 이용하여 일반적으로 작동한다. 반올림하고자 하는 양수 float에 0.5를 더하면 다음 정수 값의 임계값을 넘어서서, `math.floor()`을 적용할 때 가장 가까운 정수로 내림하게 된다.

## 참고 자료
- [Lua 5.4 참조 매뉴얼: 수학 함수](https://www.lua.org/manual/5.4/manual.html#6.7)
- [Penlight Lua 라이브러리: 수학](https://github.com/lunarmodules/Penlight)
