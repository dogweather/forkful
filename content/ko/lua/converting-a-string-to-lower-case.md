---
title:                "문자열을 소문자로 변환하기"
html_title:           "Bash: 문자열을 소문자로 변환하기"
simple_title:         "문자열을 소문자로 변환하기"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 사용하나요?
문자열을 소문자로 변경하는 것은 모든 대문자를 대응하는 소문자로 변경하는 프로그래밍 작업입니다. 이를 사용하면, 대소문자를 구별하지 않는 문자열 비교와 검색을 용이하게 할 수 있습니다. 

## 어떻게 사용하나요? 
Lua에서 문자열을 소문자로 변경하는 방법은 매우 간단합니다. 다음은 문자열 대소문자 변경에 대한 간단한 코드 예제입니다:

```Lua 
s = "Hello, Lua!"
print(s:lower())
```

이 코드를 실행하면 다음과 같은 결과가 출력됩니다:

```Lua 
hello, lua!
```

즉, `:lower()` 함수를 이용하여 문자열에 있는 대문자를 소문자로 변경할 수 있습니다. 

## 깊이 들어가보기 
Lua언어에서는 문자열을 다루는 데 있어 `:lower()`와 같은 많은 편리한 함수를 제공했습니다. 이 함수는 내부적으로 각 문자의 ASCII 값을 이용하여 변환을 실시합니다. 만약 대문자 변환을 원하는 경우에는 `:upper()` 함수를 사용할 수 있습니다.

```Lua 
s = "Hello, Lua!"
print(s:upper())
``` 
결과는 `HELLO, LUA!`가 출력될 것입니다.

## 참고자료 
다음 링크에서 Lua 프로그래밍 언어에 대한 보다 상세한 정보를 찾아볼 수 있습니다:
[Lua 5.4 레퍼런스 매뉴얼](https://www.lua.org/manual/5.4/)
[Lua 문자열 라이브러리](https://www.lua.org/manual/5.4/manual.html#6.4)