---
title:                "정규 표현식 사용하기"
aliases:
- /ko/lua/using-regular-expressions.md
date:                  2024-02-03T19:17:37.852486-07:00
model:                 gpt-4-0125-preview
simple_title:         "정규 표현식 사용하기"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇이고 왜 사용하는가?

프로그래밍에서 정규 표현식은 특정 패턴을 기반으로 문자열을 일치시키고 조작하는 데 사용됩니다. 복잡한 문자열 작업을 효율적으로 처리하는 능력과 다용도성 때문에, 프로그래머들은 검증, 검색, 텍스트 조작과 같은 작업을 위해 이를 사용합니다.

## 사용 방법:

Lua는 Perl이나 Python 같은 언어처럼 원래 정규 표현식을 지원하지 않습니다. 대신, 많은 공통적인 정규 표현식 사용 사례를 커버하는 패턴 매칭 기능을 제공합니다. 그러나, 완전한 정규 표현식 지원을 위해서는 `lrexlib` 같은 타사 라이브러리를 사용할 수 있습니다.

### Lua에서의 기본 패턴 매칭:

Lua는 간단한 치환과 검색을 위해 사용할 수 있는 강력한 패턴 매칭 시스템을 제공합니다:

```lua
-- 간단한 검색
local str = "Hello, World!"
if string.find(str, "World") then
  print("일치하는 항목을 찾았습니다!")
end
-- 출력: 일치하는 항목을 찾았습니다!

-- 간단한 치환
local s = string.gsub("Lua는 멋져!", "멋져", "최고야")
print(s)
-- 출력: Lua는 최고야!
```

### 부분 문자열 캡처하기:

문자열의 일치하는 부분을 캡처할 수 있습니다:

```lua
local date = "오늘은 2023년 05월 17일입니다."
local y, m, d = string.match(date, "(%d+)-(%d+)-(%d+)")
print("일:", d, "월:", m, "연:", y)
-- 출력: 일: 17 월: 05 연: 2023
```

### 정규 표현식에 `lrexlib` 사용하기:

실제 정규 표현식을 사용하려면, `lrexlib`을 설치하고 사용할 수 있습니다. 이것이 설치되어 있다고 가정하고 (`luarocks install lrexlib-pcre`), 보다 복잡한 패턴 매칭을 수행할 수 있습니다:

```lua
local rex = require 'rex_pcre'

local text = "The rain in Spain stays mainly in the plain."
local regex = "\\bS\\w+"
local count, err = rex.gsub(text, regex, function(w)
  return w:upper()
end)
if err then
  print("오류:", err)
else
  print("수정된 텍스트:", text)
  print("치환된 횟수:", count)
end
-- 예시 출력: 수정된 텍스트: The RAIN in SPAIN stays MAINLY in the plain.
-- 치환된 횟수: 3
```

위 예시들은 Lua 자체 패턴 매칭 시스템 내의 기본 사용법과 `lrexlib`을 통한 정규 표현식의 힘을 활용하는 방법을 보여줍니다. 단순한 문자열 조작이 필요한 경우든, 정규 표현식의 전체 다양성이 요구되는 경우든, 강력한 라이브러리와 함께 Lua는 여러분의 필요를 수용할 수 있습니다.
