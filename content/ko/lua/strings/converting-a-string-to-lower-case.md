---
aliases:
- /ko/lua/converting-a-string-to-lower-case/
date: 2024-01-20 17:38:49.461120-07:00
description: "\uBB38\uC790\uC5F4\uC744 \uC18C\uBB38\uC790\uB85C \uBCC0\uD658\uD55C\
  \uB2E4\uB294 \uAC83\uC740, \uC608\uB97C \uB4E4\uC5B4 'HELLO'\uB97C 'hello'\uB85C\
  \ \uBC14\uAFB8\uB294 \uAC83\uC744 \uB9D0\uD569\uB2C8\uB2E4. \uC774\uB97C \uD1B5\uD574\
  \ \uB300\uC18C\uBB38\uC790\uB97C \uBB34\uC2DC\uD558\uACE0 \uB370\uC774\uD130\uB97C\
  \ \uBE44\uAD50\uD558\uAC70\uB098 \uC815\uB82C\uD558\uB294 \uB4F1\uC758 \uC791\uC5C5\
  \ \uC2DC \uC77C\uAD00\uC131\uC744 \uC9C0\uD0AC \uC218 \uC788\uC2B5\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: 2024-02-18 23:09:06.397084
model: gpt-4-1106-preview
summary: "\uBB38\uC790\uC5F4\uC744 \uC18C\uBB38\uC790\uB85C \uBCC0\uD658\uD55C\uB2E4\
  \uB294 \uAC83\uC740, \uC608\uB97C \uB4E4\uC5B4 'HELLO'\uB97C 'hello'\uB85C \uBC14\
  \uAFB8\uB294 \uAC83\uC744 \uB9D0\uD569\uB2C8\uB2E4. \uC774\uB97C \uD1B5\uD574 \uB300\
  \uC18C\uBB38\uC790\uB97C \uBB34\uC2DC\uD558\uACE0 \uB370\uC774\uD130\uB97C \uBE44\
  \uAD50\uD558\uAC70\uB098 \uC815\uB82C\uD558\uB294 \uB4F1\uC758 \uC791\uC5C5 \uC2DC\
  \ \uC77C\uAD00\uC131\uC744 \uC9C0\uD0AC \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4\uC744 \uC18C\uBB38\uC790\uB85C \uBCC0\uD658\uD558\uAE30"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
문자열을 소문자로 변환한다는 것은, 예를 들어 'HELLO'를 'hello'로 바꾸는 것을 말합니다. 이를 통해 대소문자를 무시하고 데이터를 비교하거나 정렬하는 등의 작업 시 일관성을 지킬 수 있습니다.

## How to: (방법)
Lua에서 문자열을 소문자로 변환하기 위해 `string.lower()` 함수를 사용합니다. 그리고 변환 결과를 확인할 수 있습니다.

```Lua
local originalString = "Hello, World!"
local lowerString = string.lower(originalString)

print(lowerString)  -- 출력: hello, world!
```

단순하고 직관적이죠.

## Deep Dive (깊이 있게)
문자열을 소문자로 변환이 필요한 상황은 생각보다 많습니다. 예를 들면, 사용자 입력을 처리할 때 대소문자에 상관없이 동일하게 처리해야 할 경우가 있죠.

역사적으로 봤을 때, 문자열을 소문자로 바꾸는 기능은 컴퓨터 프로그래밍의 초창기부터 필요했습니다. 하지만 지역적 언어 설정(Locales)에 따라 문자 변환이 달라지므로, Lua의 `string.lower()`는 주로 ASCII 문자만을 고려합니다. 만약 다른 언어의 문자셋을 사용한다면, 이 기능은 기대하는 결과를 주지 못할 수도 있습니다.

대안으로 `utf8` 라이브러리 (Lua 5.3 이상)를 사용하면 유니코드 문자열을 처리할 때 효과적입니다:

```Lua
local utf8String = "안녕하세요!"
local lowerUtf8String = utf8.lower(utf8String)

print(lowerUtf8String)  -- 출력: 안녕하세요!
```

이 예제에서 볼 수 있듯이, 한글은 대소문자 구분이 없으니 변환 전과 후가 같습니다.

소문자 변환 구현 시 한 가지 고려할 점은 성능입니다. 대규모 데이터를 처리할 때 `string.lower()` 호출은 시간이 많이 걸릴 수 있으므로, 필요한 경우에만 변환을 수행하는 것이 좋습니다.

## See Also (더보기)
- Lua 5.4 Reference Manual: [String Manipulation](https://www.lua.org/manual/5.4/manual.html#6.4)
- GitHub Lua: [Lua Source](https://github.com/lua/lua)

이 글을 읽으신 후에 해당 매뉴얼과 튜토리얼을 참고하시면 Lua에서 문자열을 다루는 더 많은 정보를 얻을 수 있습니다.
