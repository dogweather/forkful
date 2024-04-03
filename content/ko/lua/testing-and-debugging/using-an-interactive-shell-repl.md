---
date: 2024-01-26 04:16:10.860261-07:00
description: "REPL\uC740 Read-Eval-Print Loop\uC758 \uC57D\uC790\uB85C, \uCF54\uB4DC\
  \uB97C \uBE60\uB974\uAC8C \uD14C\uC2A4\uD2B8\uD560 \uC218 \uC788\uB294 \uC0C1\uD638\
  \uC791\uC6A9 \uD658\uACBD\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\
  \uC774 \uC2E4\uD5D8, \uB514\uBC84\uAE45 \uBC0F \uC5B8\uC5B4\uC758 \uD2B9\uC774\uC810\
  \uC744 \uBC30\uC6B0\uAE30 \uC704\uD574 \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.419168-06:00'
model: gpt-4-0125-preview
summary: "REPL\uC740 Read-Eval-Print Loop\uC758 \uC57D\uC790\uB85C, \uCF54\uB4DC\uB97C\
  \ \uBE60\uB974\uAC8C \uD14C\uC2A4\uD2B8\uD560 \uC218 \uC788\uB294 \uC0C1\uD638\uC791\
  \uC6A9 \uD658\uACBD\uC785\uB2C8\uB2E4."
title: "\uC778\uD130\uB799\uD2F0\uBE0C \uC178 (REPL) \uC0AC\uC6A9\uD558\uAE30"
weight: 34
---

## 사용 방법:
Lua의 REPL로 접속하려면, 터미널에 `lua`를 입력하기만 하면 됩니다. 예시 세션은 다음과 같습니다:

```Lua
> x = 10
> print(x * 2)
20
> t = {'apple', 'banana', 'cherry'}
> table.insert(t, 'date')
> for i, fruit in ipairs(t) do print(i, fruit) end
1	apple
2	banana
3	cherry
4	date
>
```
세션에서 우리는 변수를 선언하고, 기본 산술을 수행하고, 테이블을 조작하며, 그 항목들을 반복합니다.

## 깊이 들어가기
Lua의 경량성은 프로토타이핑에 이상적인 REPL을 만듭니다. Lua의 REPL은 1990년대 초 Lua의 시작부터 있었으며, Lisp 같은 언어들을 위한 이전의 상호작용 셸에서 영감을 받았습니다. 다른 언어에서의 대안으로는 Ruby의 `irb`와 Python의 `python` 등이 있으며, 각각 고유의 기능 세트를 가지고 있습니다. Lua의 REPL은 미니멀리즘을 지향하므로, 복잡한 디버깅 도구 같은 고급 기능이 부족할 수 있습니다. 더 풍부한 경험을 위해, ZeroBrane Studio나 LuaDist의 LuaRocks 같은 도구들이 기본 REPL보다 더 많은 것을 제공합니다.

## 또한 참고하세요
- [Lua 5.4 참조 매뉴얼 - 스탠드얼론 Lua 인터프리터](https://www.lua.org/manual/5.4/manual.html#6)
- [ZeroBrane Studio](https://studio.zerobrane.com/)
- [LuaRocks](https://luarocks.org/)
