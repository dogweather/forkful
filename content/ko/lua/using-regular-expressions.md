---
title:                "정규 표현식 활용하기"
date:                  2024-01-19
html_title:           "Arduino: 정규 표현식 활용하기"
simple_title:         "정규 표현식 활용하기"

category:             "Lua"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (무엇이며 왜 사용하는가?)
정규 표현식은 문자열 패턴을 찾고 조작하는 방법이다. 프로그래머들은 코드 간결화, 데이터 검증, 검색 및 대체 작업을 위해 이를 사용한다.

## How to: (사용법)
Lua에서 정규 표현식은 패턴 매칭 기능을 사용해 구현된다. 여기 몇 가지 예제가 있다:

```Lua

-- 문자열 검색
local text = "Lua를 배웁시다"
if string.match(text, "배웁시다") then
  print("일치하는 단어가 있습니다!")
end

-- 출력: 일치하는 단어가 있습니다!

-- 대체
local text = "Lua는 멋지다"
local new_text = string.gsub(text, "멋지다", "재미있다")
print(new_text)

-- 출력: Lua는 재미있다

-- 패턴 매칭
local date = "오늘 날짜는 2023-04-01입니다."
for year, month, day in string.gmatch(date, "(%d+)-(%d+)-(%d+)") do
  print(year, month, day)
end

-- 출력:
-- 2023    04    01

```

## Deep Dive (심층 분석)
Lua에서 정규 표현식의 개념은 다른 언어의 정규 표현식처럼 포괄적이지 않고 '패턴 매칭'으로 알려져 있다. 1993년에 Lua가 처음 등장했을 때부터 패턴 매칭은 문자열 작업의 중요한 부분이었다. Lua의 패턴 매칭은 POSIX 또는 Perl과 같은 전통적인 정규 표현식 엔진만큼 강력하지 않지만, 대부분의 일상적인 작업에는 충분하다. 대안으로 Lua 패턴 매칭보다 더 복잡한 작업이 필요하다면 PCRE(Perl 호환 정규 표현식) 라이브러리와 같은 외부 라이브러리를 사용할 수 있다.

## See Also (관련 링크)
- Lua 5.4 매뉴얼: https://www.lua.org/manual/5.4/
- Lua-users Wiki: http://lua-users.org/wiki/
- 패턴 매칭 튜토리얼: http://lua-users.org/wiki/PatternsTutorial
- PCRE (Perl 호환 정규 표현식) 라이브러리: https://www.pcre.org/
