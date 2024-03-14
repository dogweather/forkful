---
date: 2024-01-20 18:04:52.121074-07:00
description: "\uD14D\uC2A4\uD2B8 \uD30C\uC77C\uC744 \uC77D\uB294\uB2E4\uB294 \uAC83\
  \uC740 \uD30C\uC77C\uC758 \uB0B4\uC6A9\uC744 \uC77D\uC5B4 \uB4E4\uC5EC \uB370\uC774\
  \uD130\uB97C \uC0AC\uC6A9\uD560 \uC218 \uC788\uAC8C \uD568\uC744 \uC758\uBBF8\uD569\
  \uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC124\uC815, \uB370\uC774\
  \uD130 \uAD50\uD658, \uB85C\uADF8 \uBD84\uC11D \uB4F1\uC744 \uC704\uD574 \uC774\
  \ \uC791\uC5C5\uC744 \uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.442572-06:00'
model: gpt-4-1106-preview
summary: "\uD14D\uC2A4\uD2B8 \uD30C\uC77C\uC744 \uC77D\uB294\uB2E4\uB294 \uAC83\uC740\
  \ \uD30C\uC77C\uC758 \uB0B4\uC6A9\uC744 \uC77D\uC5B4 \uB4E4\uC5EC \uB370\uC774\uD130\
  \uB97C \uC0AC\uC6A9\uD560 \uC218 \uC788\uAC8C \uD568\uC744 \uC758\uBBF8\uD569\uB2C8\
  \uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC124\uC815, \uB370\uC774\uD130\
  \ \uAD50\uD658, \uB85C\uADF8 \uBD84\uC11D \uB4F1\uC744 \uC704\uD574 \uC774 \uC791\
  \uC5C5\uC744 \uD569\uB2C8\uB2E4."
title: "\uD14D\uC2A4\uD2B8 \uD30C\uC77C \uC77D\uAE30"
---

{{< edit_this_page >}}

## What & Why? (무엇이며, 왜?)
텍스트 파일을 읽는다는 것은 파일의 내용을 읽어 들여 데이터를 사용할 수 있게 함을 의미합니다. 프로그래머들은 설정, 데이터 교환, 로그 분석 등을 위해 이 작업을 합니다.

## How to: (어떻게 하나요?)
```Lua
-- 파일 열기, 읽기 모드('r')로 설정
local file = io.open("sample.txt", "r")

-- 파일이 성공적으로 열렸는지 확인
if not file then
  error("파일을 열 수 없습니다!")
end

-- 파일의 모든 내용을 읽기
local content = file:read("*a")

-- 내용 출력
print(content)

-- 파일 닫기
file:close()
```

Sample output:
```
안녕하세요, Lua 학습자 여러분!
오늘은 파일 읽기에 대해 배워볼 거예요.
```

## Deep Dive (심층 분석)
Lua에서 파일을 다루는 것은 io 라이브러리를 통해 이루어집니다. 이 라이브러리는 ANSI C 표준을 기반으로 작성되었으며, Lua의 초기 버전부터 존재했습니다. `io.open` 함수는 파일을 열 때 사용되며, 다른 모드('w' - 쓰기, 'a' - 추가 등)도 지원합니다. Lua 5.1 이후에는 더 나은 에러 핸들링을 위해 `io.lines`와 `file:lines` 함수도 추가되었습니다. 대안으로는 `os.execute`를 이용해 외부 프로그램을 사용하는 방법도 있지만, 권장되지는 않습니다. 또한, LuaJIT와 같은 구현체는 io 라이브러리의 성능을 향상시켜 주기도 합니다.

## See Also (참고 자료)
- [Lua 5.4 Reference Manual - io library](https://www.lua.org/manual/5.4/manual.html#6.8)
