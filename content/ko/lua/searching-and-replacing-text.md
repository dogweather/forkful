---
title:                "텍스트 검색 및 교체"
html_title:           "Elixir: 텍스트 검색 및 교체"
simple_title:         "텍스트 검색 및 교체"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 필요한가?

텍스트 검색 및 교체는 문자열 내에서 특정 텍스트를 찾고 해당 텍스트를 다른 텍스트로 바꾸는 작업이다. 이는 데이터 분석, 웹 스크래핑, 파일 수정 등 다양한 프로그래밍 상황에서 필수적인 작업이다.

## 어떻게 할 것인가?

Lua에서 문자열을 검색하고 교체하는 것은 `gsub` 함수를 사용하여 가능하다. 아래는 코드 예시와 그 출력 결과이다.

```Lua
local str1 = "안녕하세요, Lua를 배우고 있는 중입니다."
local str2 = string.gsub(str1, "Lua", "파이썬")
print(str2) -- 출력: "안녕하세요, 파이썬를 배우고 있는 중입니다."
```

## 깊이 들어가보기

`gsub` 함수는 Lua 4.0에서 도입되었으며, 이 함수는 비교적 간단한 문제에 대한 간단한 해결책을 제공한다. 다양한 작업에 적용할 수 있도록 폭넓은 매개 변수를 받을 수 있다.

대안으로는 패턴 일치 및 교체에 더욱 강력한 도구를 제공하는 Lua의 패턴 매칭 라이브러리나 외부 라이브러리를 사용하는 방법이 있다.

`gsub` 의 기본 구현은 `luaS_gmatch` 함수 안에서 이루어진다. 이 함수는 일치하는 모든 패턴을 순회하며, 각각에 대해 치환 함수를 실행한다.

## 관련 자료

더 깊이 공부하고 싶다면 아래의 링크를 확인해보세요.

1. Lua 공식 문서의 `gsub` 설명: https://www.lua.org/manual/5.4/manual.html#pdf-string.gsub
2. Lua 패턴 매칭 튜토리얼: http://lua-users.org/wiki/PatternsTutorial
3. Lua에서 정규 표현식 사용 방법: https://pleiades.io/help/idea/lua-regular-expressions.html