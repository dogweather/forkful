---
title:                "부분 문자열 추출"
aliases:
- /ko/lua/extracting-substrings.md
date:                  2024-01-20T17:46:04.775553-07:00
model:                 gpt-4-1106-preview
simple_title:         "부분 문자열 추출"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
문자열에서 부분 문자열 추출하기는 필요한 정보를 얻기 위해 문자열의 특정 부분을 뽑아내는 것입니다. 데이터 처리나 텍스트 분석에서 중요한 부분이죠.

## How to: (어떻게 하나요?)
Lua에서 부분 문자열을 추출하는 기본 함수는 `string.sub`입니다. 사용법을 보여주는 간단한 예시들을 봅시다.

```lua
local text = "Hello, Lua!"

-- 인덱스 8부터 끝까지 추출하기
print(string.sub(text, 8))  -- Lua!

-- 인덱스 1부터 5까지 추출하기
print(string.sub(text, 1, 5))  -- Hello

-- 마지막 4글자 추출하기
print(string.sub(text, -4))  -- Lua!
```

출력 결과:
```
Lua!
Hello
Lua!
```

## Deep Dive (심층 분석)
`string.sub` 함수는 Lua 5부터 사용 가능합니다. 이 함수 외에도 `string.match`를 정규 표현식과 함께 쓰면 더 세밀한 추출이 가능해져요. 예를 들어:

```lua
local date = "2023-04-01"
local year, month, day = string.match(date, "(%d+)-(%d+)-(%d+)")
print(year, month, day)  -- 2023 04 01
```

성능 면에서 보면, `string.sub`은 C 레벨에서 구현되어 있어서 빠릅니다. 문자열 길이가 길어질수록, 그 차이는 더욱 명확해져요.

## See Also (관련 자료)
- Lua 5.4 참조 매뉴얼: [string.sub](https://www.lua.org/manual/5.4/manual.html#pdf-string.sub)
- 복잡한 패턴 매칭 예제: [string.match](https://www.lua.org/manual/5.4/manual.html#pdf-string.match)
