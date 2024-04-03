---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:37.852486-07:00
description: "\uC0AC\uC6A9 \uBC29\uBC95: Lua\uB294 Perl\uC774\uB098 Python \uAC19\uC740\
  \ \uC5B8\uC5B4\uCC98\uB7FC \uC6D0\uB798 \uC815\uADDC \uD45C\uD604\uC2DD\uC744 \uC9C0\
  \uC6D0\uD558\uC9C0 \uC54A\uC2B5\uB2C8\uB2E4. \uB300\uC2E0, \uB9CE\uC740 \uACF5\uD1B5\
  \uC801\uC778 \uC815\uADDC \uD45C\uD604\uC2DD \uC0AC\uC6A9 \uC0AC\uB840\uB97C \uCEE4\
  \uBC84\uD558\uB294 \uD328\uD134 \uB9E4\uCE6D \uAE30\uB2A5\uC744 \uC81C\uACF5\uD569\
  \uB2C8\uB2E4. \uADF8\uB7EC\uB098, \uC644\uC804\uD55C \uC815\uADDC \uD45C\uD604\uC2DD\
  \ \uC9C0\uC6D0\uC744 \uC704\uD574\uC11C\uB294 `lrexlib` \uAC19\uC740 \uD0C0\uC0AC\
  \ \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C\u2026"
lastmod: '2024-03-13T22:44:55.403091-06:00'
model: gpt-4-0125-preview
summary: "Lua\uB294 Perl\uC774\uB098 Python \uAC19\uC740 \uC5B8\uC5B4\uCC98\uB7FC\
  \ \uC6D0\uB798 \uC815\uADDC \uD45C\uD604\uC2DD\uC744 \uC9C0\uC6D0\uD558\uC9C0 \uC54A\
  \uC2B5\uB2C8\uB2E4."
title: "\uC815\uADDC \uD45C\uD604\uC2DD \uC0AC\uC6A9\uD558\uAE30"
weight: 11
---

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
