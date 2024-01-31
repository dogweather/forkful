---
title:                "테스트 작성하기"
date:                  2024-01-19
simple_title:         "테스트 작성하기"

tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
테스트 작성은 코드가 예상대로 작동하는지 확인하는 과정입니다. 버그를 미리 찾아내고, 코드의 안정성을 향상시키기 위해 프로그래머들은 테스트를 합니다.

## How to: (어떻게 하나요?)
```Lua
-- simple_test.lua
local function add(a, b)
    return a + b
end

-- 테스트 함수
local function testAddFunction()
    local result = add(5, 3)
    assert(result == 8, "5와 3을 더하면 8이 나와야 합니다.")
end

-- 테스트 실행
testAddFunction()
print("테스트 통과: 5와 3의 합은 8입니다.")
```
```
테스트 통과: 5와 3의 합은 8입니다.
```

## Deep Dive (심도있는 분석)
테스트 작성은 1970년대부터 있어왔지만, 1990년대 TDD(Test-Driven Development, 테스트 주도 개발)와 함께 더 중요해졌습니다. 유닛 테스트, 통합 테스트 등 다양한 테스트 방법이 있습니다. Lua에서는 `LuaUnit`, `busted` 같은 테스트 프레임워크를 사용해 복잡한 테스트를 쉽게 작성할 수 있습니다.

## See Also (관련 자료)
- [LuaUnit 공식문서](https://luaunit.readthedocs.io/en/latest/)
- [Test-Driven Development](https://en.wikipedia.org/wiki/Test-driven_development)
