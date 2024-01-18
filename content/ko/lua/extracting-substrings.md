---
title:                "부분 문자열 추출"
html_title:           "Lua: 부분 문자열 추출"
simple_title:         "부분 문자열 추출"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/extracting-substrings.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

서브스트링 추출이란 무엇이고 프로그래머들이 왜 이것을 하는지 간단히 설명해보겠습니다. 서브스트링 추출은 문자열에서 원하는 부분만을 따로 추출하는 것을 의미합니다. 이를테면, "Hello World"라는 문자열에서 "World"라는 부분만을 추출하는 것입니다. 프로그래머들은 서브스트링 추출을 사용하여 문자열을 다루기 더 편리하고 유연하게 만들고, 원하는 정보만을 추출하여 처리하는 데 시간과 노력을 절약할 수 있습니다.

## 어떻게:

```Lua
local str = "Hello World"
-- string.sub() 함수 사용하여 서브스트링 추출하기
print(string.sub(str, 7, 11)) -- 결과: "World"
-- 인덱스를 사용하여 직접 추출하기
print(str:sub(7, 11)) -- 결과: "World"
```
출력:
```
World
World
```

## 심층 탐구:

1. 역사적 배경: 서브스트링 추출은 오래 전부터 이용되어온 기술이며, 문자열을 다루는 데 있어서 필수적인 기능 중 하나입니다.
2. 대안: 서브스트링 추출을 위해 문자열의 일부를 복제하는 방식 대신, 문자열의 일부를 가리키는 포인터 개념을 사용하는 언어들도 있습니다.
3. 구현 세부 사항: Lua에서는 문자열 내부에 있는 개별 문자를 인덱스로 접근할 수 있도록 하는 문자열 라이브러리를 제공합니다. 이를 바탕으로 내장 함수인 ```string.sub()``` 함수를 사용하여 서브스트링을 추출할 수 있습니다.

## 관련 자료:

- [Lua 공식 문서 - string 라이브러리](https://www.lua.org/pil/20.html)
- [직접 해보는 Lua 프로그래밍 - 문자열 다루기](https://edykim.com/ko/post/string-processing-in-lua-part1/)