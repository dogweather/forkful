---
title:                "패턴과 일치하는 문자 삭제"
html_title:           "Lua: 패턴과 일치하는 문자 삭제"
simple_title:         "패턴과 일치하는 문자 삭제"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

문자열에서 패턴과 일치하는 문자를 제거하는 것은 프로그래머들이 자주 사용하는 작업입니다. 이를 통해 불필요한 문자를 삭제하거나 원하는 패턴으로 문자열을 정리할 수 있습니다. 

## 방법:

```Lua
-- 문자열에서 "a"를 제거하는 예시
local str = "apple"
str = string.gsub(str, "a", "")
print(str)
-- output: "pple"
```

```Lua
-- 숫자가 아닌 문자를 제거하는 예시
local str = "1a2b3c4d"
str = string.gsub(str, "%D", "")
print(str)
-- output: "1234"
```

## 심층 분석:

- 역사적 배경: 문자열에서 패턴을 제거하는 기능은 오래된 시점부터 사용되어 왔으며, 프로그래머들의 요구에 따라 계속 발전해왔습니다.
- 대안: 문자열에서 패턴을 제거하는 다른 방법으로는 정규식(regular expressions)을 사용할 수 있습니다.
- 구현 세부 정보: ```string.gsub()``` 함수는 문자열에서 패턴을 검색하고 일치하는 문자를 다른 문자로 대체하는 기능을 수행합니다. 

## 관련 자료:

- [Lua 공식 문서](https://www.lua.org/docs.html)
- [정규식 테스트 사이트](https://regex101.com/)