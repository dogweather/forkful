---
title:                "문자열 보간하기"
html_title:           "Clojure: 문자열 보간하기"
simple_title:         "문자열 보간하기"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

문자열 보간(string interpolation)은 문자열 내에서 변수나 표현식을 동적으로 대입하는 것을 말합니다. 이를 통해 코드를 더 간결하고 읽기 쉽게 만들 수 있습니다.

## 어떻게 할까?

Lua에서 문자열 보간을 하려면 기본적으로 `string.format` 함수를 사용합니다. 작성된 예제로 살펴보겠습니다.

```Lua
local name = "Lua"
local message = string.format("Hello, %s", name)
print(message)  -- 'Hello, Lua' 출력
```

`%s`는 문자열 값을, `%d`는 정수 값을 대입하는 데 사용됩니다.

```Lua
local age = 20
local message = string.format("나이: %d", age)
print(message)  -- '나이: 20' 출력
```

## 깊게 알아보기

Lua에서 문자열 보간에 대해 알아보면 이 기능이 다른 언어에서는 어땠는지 궁금해질 수 있습니다. 예를 들어, JavaScript와 Python 같은 언어들은 "string interpolation"문법이 별도로 있습니다.

```Lua
-- Lua에서는 존재하지 않는 기능
local name = "Lua"
local message = `Hello, ${name}`  -- JavaScript
local message = f"Hello, {name}"  -- Python
```

Lua는 더 간단하고 빠르게 구현될 수 있도록, 개발 초기부터 이런 복잡한 기능은 배제하였습니다. 따라서 Lua에서 문자열 보간을 사용하려면, `string.format` 함수나 비슷한 함수를 사용하거나, 직접 구현해야 합니다.

## 참고 자료

- [Lua 사용자 위키: 문자열 보간](http://lua-users.org/wiki/StringInterpolation)
- [Programming in Lua (Fourth Edition): 포맷 문자열](https://www.lua.org/manual/5.4/manual.html#6.4.2)