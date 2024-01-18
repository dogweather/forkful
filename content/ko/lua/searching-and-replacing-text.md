---
title:                "텍스트 검색 및 대체"
html_title:           "Lua: 텍스트 검색 및 대체"
simple_title:         "텍스트 검색 및 대체"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Lua에서 텍스트 검색과 대체하기

## 무엇 & 왜?

텍스트 검색 및 대체는 프로그래머들이 자주 하는 작업입니다. 이 작업은 특정 문자열을 찾아 바꾸는 것을 의미합니다. 예를 들어, 코드에서 많은 곳에서 사용되는 변수 이름을 한번에 모두 변경하려면 검색 및 대체 기능을 사용할 수 있습니다. 이를 통해 작업 속도와 정확성이 향상될 수 있습니다.

## 하는 법:

검색 및 대체는 루아에서 매우 간단한 작업입니다. 다음 코드 예제를 참고해보세요.

```
-- 문자열에서 "Apple"을 찾아 "Banana"로 바꾸는 예제
local str = "I love Apple, do you love it too?"
print(str:gsub("Apple", "Banana"))
```
출력: I love Banana, do you love it too?

```
-- 변수 이름을 한번에 변경하는 예제
local old_name = "xValue"
local new_name = old_name:gsub("x", "y")
print(new_name)
```
출력: yValue


## 깊이 파고들기:

검색 및 대체 기능은 대부분의 프로그래밍 언어에서 기본적으로 제공되는 기능입니다. 루아에서는 문자열 간의 대체만 가능하며, 정규식을 사용할 수 없습니다. 그러나 정규식을 사용하지 않더라도 지원하는 능력으로 대부분의 경우 충분합니다.

비슷한 기능으로는 문자열 분할/조인, 문자열 길이/절사 등이 있습니다.

루아에서는 대부분의 문자열 관련 기능이 문자열 객체의 메서드로 지원됩니다. 이를 활용하면 문자열 처리 작업이 쉽고 간편해집니다.

## 관련 자료:

루아 공식 문서의 문자열 관련 부분: https://www.lua.org/manual/5.4/manual.html#6.4.1

루아에서 문자열 처리 기능을 활용해볼 수 있는 유용한 예제들: https://gist.github.com/baor/9079000