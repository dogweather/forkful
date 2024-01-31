---
title:                "문자열에서 날짜 파싱하기"
date:                  2024-01-20T15:37:22.659357-07:00
simple_title:         "문자열에서 날짜 파싱하기"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
날짜 파싱은 문자열에서 날짜 정보를 추출하는 과정입니다. 프로그래머들은 데이터를 정렬, 저장, 또는 날짜와 시간 기능을 실행하기 위해 이를 수행합니다.

## How to: (어떻게 하나요?)
```Lua
-- 날짜 문자열 파싱하기
local dateString = "2023-04-12 23:20:00"
-- 패턴에 맞추어 연, 월, 일, 시, 분, 초를 추출
local pattern = "(%d+)%-(%d+)%-(%d+) (%d+):(%d+):(%d+)"
local year, month, day, hour, minute, second = dateString:match(pattern)
print(year, month, day, hour, minute, second)
```
출력:
```
2023 04 12 23 20 00
```

## Deep Dive (심층 분석)
날짜 파싱은 초기 프로그래밍 언어 개발 시부터 필요했습니다. Lua에서는 간단한 패턴 매칭을 사용하여 문자열로부터 날짜를 추출할 수 있지만, 복잡한 날짜 처리를 위해서는 `os.date`와 `os.time` 같은 내장 함수나 외부 라이브러리를 활용할 수도 있습니다. 예를 들어, `os.date("*t")`는 현재 날짜와 시간을 테이블로 반환합니다. Lua에서 문자열을 통해 날짜를 파싱하면, 다양한 시간대나 형식의 일관성을 확보하고 에러를 줄일 수 있습니다.

패턴 매칭을 사용하는 것은 간소화된 방법이며, 애플리케이션의 요구에 따라 더 정교한 파싱 방식이 필요할 수 있습니다. 예를 들어, Lua에서는 POSIX `strptime` 함수에 해당하는 내장 기능이 없으므로, 이를 직접 구현하거나 다른 날짜 파싱 라이브러리를 찾아야 할 수도 있습니다.

## See Also (참고 자료)
- [Lua 5.4 Reference Manual](https://www.lua.org/manual/5.4/) - Lua 언어와 내장 함수에 대한 공식 문서.
- [LuaDate](https://github.com/Tieske/date) - 복잡한 날짜 계산을 위한 Lua 라이브러리.
- [LuaRocks](https://luarocks.org/) - Lua 라이브러리를 찾고 관리하는데 도움을 주는 패키지 매니저.
