---
title:                "테스트 작성하기"
html_title:           "Lua: 테스트 작성하기"
simple_title:         "테스트 작성하기"
programming_language: "Lua"
category:             "Lua"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/writing-tests.md"
---

{{< edit_this_page >}}

# 테스트 작성하기 & 왜 필요한가?

테스트 작성이란 무엇인가? 테스트를 작성하는 것은 코드를 실행해서 어떤 동작을 하는지 확실하게 하기 위한 과정입니다. 프로그래머들이 이를 하는 이유는 코드의 버그를 발견하고 수정하기 쉽게 하기 위함입니다.

# 어떻게 하는가?

```lua
-- 예시 코드
-- 함수 정의
function add(x, y)
  return x + y
end

-- 테스트 코드
assert(add(2, 2) == 4, "2와 2를 더한 결과는 4가 되어야만 합니다!")  -- 테스트를 통과하지 못하면 오류 메시지 출력
```

출력: 테스트를 통과하지 못하면 오류가 발생하지만, 모든 테스트를 통과할 경우 아무런 메시지도 출력되지 않습니다.

# 더 깊게 알아보기

1. 역사적 배경: 최초의 테스트 프레임워크는 1960년대에 등장한 "디버그 프로그램"이었습니다.
2. 대안들: 다른 프로그래밍 언어에서도 테스트 작성을 위한 다양한 도구들이 있지만, Lua에서는 assert 함수를 사용할 수 있습니다.
3. 구현 상세: assert 함수는 조건문을 통해 코드의 결과가 원하는 값과 일치하는지를 확인하며, 테스트를 통과하거나 오류를 출력합니다.

# 관련 자료

- [Lua 공식 홈페이지](https://www.lua.org/): Lua 언어의 공식 홈페이지입니다.
- [TNT - Test Notation Toolkit](http://tnt-fdw.com/): Lua에서 사용할 수 있는 테스트 작성 도구입니다.
- [Learn Lua in 15 minutes](http://tylerneylon.com/a/learn-lua/): 15분 만에 Lua 언어를 배울 수 있는 자료입니다.