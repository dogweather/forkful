---
date: 2024-01-20 17:42:34.038572-07:00
description: "How to: (\uBC29\uBC95) Lua\uC5D0\uC11C \uBB38\uC790\uC5F4\uC5D0\uC11C\
  \ \uD328\uD134\uC744 \uC0AC\uC6A9\uD574 \uBB38\uC790\uB97C \uC0AD\uC81C\uD558\uB824\
  \uBA74 `string.gsub` \uD568\uC218\uB97C \uC0AC\uC6A9\uD558\uC138\uC694."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.395407-06:00'
model: gpt-4-1106-preview
summary: "Lua\uC5D0\uC11C \uBB38\uC790\uC5F4\uC5D0\uC11C \uD328\uD134\uC744 \uC0AC\
  \uC6A9\uD574 \uBB38\uC790\uB97C \uC0AD\uC81C\uD558\uB824\uBA74 `string.gsub` \uD568\
  \uC218\uB97C \uC0AC\uC6A9\uD558\uC138\uC694."
title: "\uD328\uD134\uC5D0 \uC77C\uCE58\uD558\uB294 \uBB38\uC790 \uC0AD\uC81C"
weight: 5
---

## How to: (방법)
Lua에서 문자열에서 패턴을 사용해 문자를 삭제하려면 `string.gsub` 함수를 사용하세요.
```Lua
local originalString = "Hello 123 World!"
local pattern = "%d" -- 숫자에 맞는 패턴
local cleanedString = originalString:gsub(pattern, "")
print(cleanedString)
```
출력될 내용:
```
Hello  World!
```

패턴 `[aeiou]`를 사용하여 모든 모음을 삭제해 보겠습니다.
```Lua
local stringWithVowels = "Banana"
local vowelsPattern = "[aeiou]"
local resultString = stringWithVowels:gsub(vowelsPattern, "")
print(resultString)
```
출력될 내용:
```
Bnn
```

## Deep Dive (심층 분석)
패턴 매칭을 사용한 문자 삭제는 Lua의 강력한 문자열 처리 능력 중 하나입니다. Lua에서는 Perl과 비슷한 정규 표현식이 아니라, 자체적으로 개발된 패턴 매칭 시스템을 사용합니다. 이는 메모리의 사용을 줄이고 성능을 향상시킬 수 있습니다.

정규 표현식 대신 Lua는 `%` 기호를 사용하여 특수 문자를 이스케이프합니다. 예를 들어, 숫자나 알파벳에 해당하는 문자를 찾을 때 `%d`, `%a` 와 같이 사용합니다.

`string.gsub` 외에도 문자열을 다루기 위한 다른 함수들이 있지만, 패턴에 맞는 문자를 삭제할 때는 주로 `string.gsub`를 사용합니다.

## See Also (더보기)
- Lua 5.4의 공식 문자열 패턴 매칭 문서: [http://www.lua.org/manual/5.4/manual.html#6.4.1](http://www.lua.org/manual/5.4/manual.html#6.4.1)
- Lua 문자열 함수들에 대한 더 깊은 이해를 위한 튜토리얼: [https://www.tutorialspoint.com/lua/lua_strings.htm](https://www.tutorialspoint.com/lua/lua_strings.htm)
