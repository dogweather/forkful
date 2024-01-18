---
title:                "문자열 보간"
html_title:           "Lua: 문자열 보간"
simple_title:         "문자열 보간"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/interpolating-a-string.md"
---

{{< edit_this_page >}}

# 무엇 & 왜?

문자열 보간(interpolating a string)은 문자열 안에 변수 또는 다른 값들을 삽입하는 것을 말합니다. 이렇게 함으로써 우리는 동적인 문자열을 만들 수 있고, 정보를 보다 편리하게 표시할 수 있게 됩니다. 프로그래머들이 문자열 보간을 하는 이유는 코드의 가독성을 높이고, 반복적인 작업을 줄여 더 편리하게 코드를 작성하기 위해서입니다.

# 방법:

```
-- 변수를 문자열 안에 삽입하기
local name = "John"
print("안녕하세요, " .. name .. "님!")

-- 숫자를 문자열로 변환하기
local age = 25
print("나이는 " .. tostring(age) .. "살입니다.")
```

출력:
```
안녕하세요, John님!
나이는 25살입니다.
```

# 깊이 파헤치기:

1. 역사적 맥락: 문자열 보간은 다른 언어에서도 사용되었지만, Lua에서는 ".." 연산자를 사용하여 구현됩니다.
2. 대안: 문자열 보간 대신 문자열을 연결하는 방식으로도 값을 삽입할 수 있지만, 가독성과 편의성 측면에서 문자열 보간이 더 우수합니다.
3. 구현 상세: ".." 연산자는 문자열에 대한 메타테이블을 이용하여 구현됩니다. 문자열의 concat 메소드를 변경하여 값을 삽입하도록 만듭니다.

# 참고 자료:

문자열 보간에 대한 더 자세한 내용은 아래 링크를 참고하세요.
- [Lua 문자열 보간 소개](https://www.w3schools.com/lua/lua_strings.asp)
- [메타테이블 문서](https://www.lua.org/manual/5.3/manual.html#2.4)
- [역사적 맥락에 대한 내용](https://www.lua.org/about.html)