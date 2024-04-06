---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:43.011987-07:00
description: "\uC5B4\uB5BB\uAC8C: Lua\uB294 \uAC00\uBCBC\uC6B0\uBA74\uC11C\uB3C4 \uAC15\
  \uB825\uD55C \uC2A4\uD06C\uB9BD\uD305 \uC5B8\uC5B4\uC774\uC9C0\uB9CC, \uB0B4\uC7A5\
  \uB41C \uD14C\uC2A4\uD2B8 \uD504\uB808\uC784\uC6CC\uD06C\uB97C \uD3EC\uD568\uD558\
  \uACE0 \uC788\uC9C0 \uC54A\uC2B5\uB2C8\uB2E4. \uADF8\uB7EC\uB098, Busted \uBC0F\
  \ LuaUnit\uACFC \uAC19\uC740 \uD0C0\uC0AC \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB294 \uD14C\
  \uC2A4\uD2B8\uB97C \uBE44\uAD50\uC801 \uAC04\uB2E8\uD558\uAC8C \uB9CC\uB4ED\uB2C8\
  \uB2E4. \uC5EC\uAE30\uC11C\uB294 \uB458 \uBAA8\uB450\uB97C \uC0AC\uC6A9\uD558\uC5EC\
  \ \uC608\uC81C\uB97C \uC0B4\uD3B4\uBCF4\uACA0\uC2B5\uB2C8\uB2E4."
lastmod: '2024-04-05T21:53:57.109861-06:00'
model: gpt-4-0125-preview
summary: "Lua\uB294 \uAC00\uBCBC\uC6B0\uBA74\uC11C\uB3C4 \uAC15\uB825\uD55C \uC2A4\
  \uD06C\uB9BD\uD305 \uC5B8\uC5B4\uC774\uC9C0\uB9CC, \uB0B4\uC7A5\uB41C \uD14C\uC2A4\
  \uD2B8 \uD504\uB808\uC784\uC6CC\uD06C\uB97C \uD3EC\uD568\uD558\uACE0 \uC788\uC9C0\
  \ \uC54A\uC2B5\uB2C8\uB2E4."
title: "\uD14C\uC2A4\uD2B8 \uC791\uC131\uD558\uAE30"
weight: 36
---

## 어떻게:
Lua는 가벼우면서도 강력한 스크립팅 언어이지만, 내장된 테스트 프레임워크를 포함하고 있지 않습니다. 그러나, Busted 및 LuaUnit과 같은 타사 라이브러리는 테스트를 비교적 간단하게 만듭니다. 여기서는 둘 모두를 사용하여 예제를 살펴보겠습니다.

### Busted 사용하기
Busted는 유연한 방식으로 테스트를 작성할 수 있는 인기 있는 Lua 테스트 프레임워크입니다. 우선, LuaRocks(Lua의 패키지 관리자)를 통해 `luarocks install busted`로 Busted를 설치합니다. 설치가 완료되면, 테스트를 작성할 수 있습니다. 두 수를 더하는 함수 `add`에 대한 간단한 테스트는 다음과 같습니다:

```lua
-- add.lua
local function add(a, b)
  return a + b
end

return add
```

```lua
-- add_spec.lua
local add = require('add')

describe("Add function", function()
  it("should add two numbers correctly", function()
    assert.are.equal(5, add(2, 3))
  end)
end)
```

테스트를 실행하려면 터미널에서 `busted`를 실행하세요. 통과하는 테스트의 샘플 출력은 다음과 같습니다:

```
●
1 success / 0 failures / 0 errors / 0 pending : 0.002 seconds
```

### LuaUnit 사용하기
LuaUnit은 xUnit 관례를 따르는 또 다른 테스트 프레임워크로 설정이 쉽습니다. LuaRocks를 사용하여 `luarocks install luaunit`으로 LuaUnit을 설치합니다. 위와 유사한 테스트를 LuaUnit으로 작성하는 방법은 다음과 같습니다:

```lua
-- add.lua는 그대로 유지됩니다.

-- test_add.lua
luaunit = require('luaunit')
local add = require('add')

function testAdd()
  luaunit.assertEquals(add(2, 3), 5)
end

os.exit(luaunit.LuaUnit.run())
```

이 스크립트를 Lua를 통해 직접 실행하면 (`lua test_add.lua`) 다음과 같은 출력이 나타납니다:

```
.
Ran 1 tests in 0.001 seconds, 1 success, 0 failures
```

Busted와 LuaUnit 모두 모킹, 스파이, 비동기 테스트를 포함한 다양한 테스트 시나리오를 처리할 수 있는 광범위한 기능을 제공합니다. 두 프레임워크 중 선택은 프로젝트의 구체적인 요구사항과 구문 및 기능에 대한 개인적인 선호에 달려 있습니다.
