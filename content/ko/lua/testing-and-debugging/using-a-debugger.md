---
title:                "디버거 사용하기"
aliases:
- /ko/lua/using-a-debugger/
date:                  2024-01-26T03:50:34.765954-07:00
model:                 gpt-4-0125-preview
simple_title:         "디버거 사용하기"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/using-a-debugger.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
디버거는 프로그램의 실행을 검사하고 제어할 수 있게 해주는 도구로, 문제가 발생하는 위치를 쉽게 찾아낼 수 있게 해줍니다. 프로그래머들은 버그를 제거하고, 코드 흐름을 이해하며, 코드가 깔끔하게 유지되도록 디버거를 사용합니다.

## 사용 방법:
Lua는 내장 디버거가 없지만, ZeroBrane Studio와 같은 외부 디버거를 사용할 수 있습니다. 이것이 어떻게 작동하는지 간단히 알아보겠습니다:

```Lua
-- 의도적인 오류가 있는 간단한 Lua 스크립트입니다
local function add(a, b)
    local result = a+ b -- 이런, 'b'를 정의하는 걸 잊어버렸다고 가정해 봅시다
    return result
end

print(add(10))
```

이것을 디버거에서 실행하면, 문제가 발생하는 위치에서 실행을 중단합니다. 다음과 같은 것을 볼 수 있습니다:

```
lua: example.lua:3: nil 값에 대한 산술 연산 시도 (local 'b')
스택 트레이스백:
	example.lua:3: 'add' 함수 안에서
	example.lua:7: 메인 청크에서
	[C]: in ?
```

디버거를 사용하면, 브레이크포인트를 설정하고, 코드를 단계별로 진행하며, 변수 값들을 살펴보면서 정신을 잃지 않고 버그를 추적할 수 있습니다.

## 심층 탐구
Lua의 간결함은 안타깝게도 디버깅에까지 이르지 않습니다. 하지만 걱정하지 마세요, Lua 커뮤니티가 여러분을 지원합니다. ZeroBrane Studio, LuaDec 등과 같은 도구들이 디버깅 기능을 제공합니다. 역사적으로, 디버거는 프로그램이 처음으로 문제를 일으킨 직후에 나타났으며, 개발자들이 맹목적으로 작업하지 않고도 코드를 수정할 수 있는 수단을 제공했습니다.

Lua에서는 종종 외부 디버거에 의존하거나 개발 환경에 디버거를 내장합니다. 예를 들어, ZeroBrane Studio는 Lua 디버거를 완전히 통합한 IDE입니다. 코드를 단계별로 진행하고, 브레이크포인트를 설정하며, 변수를 관찰할 수 있게 해줍니다. 구현 측면에서, 디버거는 주로 훅을 사용하여 브레이크포인트와 다른 디버깅 기능을 삽입합니다.

대안이 있나요? 물론입니다. 'printf 디버깅'으로 애정을 받는 좋은 구식 `print` 문들이 때때로 고급 도구 없이도 일을 해내곤 합니다.

## 참고하기
디버깅 여정을 계속하려면 다음을 확인하세요:

- ZeroBrane Studio: https://studio.zerobrane.com/
- Lua 사용자 위키에서 Lua 코드 디버깅: http://lua-users.org/wiki/DebuggingLuaCode
- Lua 매뉴얼의 `debug` 라이브러리 참조: https://www.lua.org/manual/5.4/manual.html#6.10
