---
title:                "표준 에러에 쓰는 방법"
html_title:           "Lua: 표준 에러에 쓰는 방법"
simple_title:         "표준 에러에 쓰는 방법"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/writing-to-standard-error.md"
---

{{< edit_this_page >}}

"## 무엇 & 왜?"
Lua에서 standard error에 쓰는 것은, 프로그래머들이 오류와 관련된 정보를 사용자에게 보여주고 디버깅을 도와주기 위해 하는 것입니다.

"## 방법:"
```Lua
-- 예제 코드
print("일반 출력")
io.stderr:write("에러 출력")
```

``` 출력:
일반 출력
에러 출력
```

"## 깊이 들어가기:"
(1) Standard error에 대해서 알아볼 때는, C 언어에서 비롯된 개념이라고 할 수 있습니다. (2) Lua에서는 다른 방식으로도 오류를 처리할 수 있지만, 그것만으로는 사용자에게 보여주는 것이 어려울 수 있기 때문에 standard error를 사용합니다. (3) 코드에서는 io.stderr 객체를 통해 standard error에 접근할 수 있습니다.

"## 관련 자료:"
- [Lua API 문서](https://www.lua.org/manual/5.4/manual.html#6.8)
- [C 언어의 Standard Error](https://www.tutorialspoint.com/c_standard_library/c_function_fprintf.htm)
- [디버깅에 대한 자세한 설명 (영어)](https://stackify.com/how-to-log-to-stdout-in-php/)