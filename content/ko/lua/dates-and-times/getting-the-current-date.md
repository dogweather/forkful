---
title:                "현재 날짜 가져오기"
aliases:
- /ko/lua/getting-the-current-date.md
date:                  2024-02-03T19:10:14.192656-07:00
model:                 gpt-4-0125-preview
simple_title:         "현재 날짜 가져오기"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?

프로그래밍에서 현재 날짜를 검색하는 것은 로깅, 이벤트 타임스탬핑 또는 작업 스케줄링을 포함한 다양한 애플리케이션에 대해 중요한 작업입니다. Lua에서 이 기능은 프로그래머가 자신의 애플리케이션 내에서 날짜 및 시간 작업을 원활하게 처리할 수 있도록 하여 소프트웨어가 실시간 데이터와 효과적으로 상호 작용할 수 있도록 합니다.

## 사용 방법:

Lua는 현재 날짜와 시간을 얻기 위해 `os.date` 함수를 제공합니다. 이 함수는 형식화된 문자열을 얻기 위해 아규먼트 없이 사용할 수 있거나 출력을 사용자 정의하기 위해 형식 지정자와 함께 사용할 수 있습니다. 사용 방법은 다음과 같습니다:

```lua
-- 형식화된 문자열로 현재 날짜와 시간 가져오기
print(os.date())  -- 예: Thu Mar  3 14:02:03 2022

-- 출력 형식 사용자 정의하기
-- %Y는 연도, %m은 월, %d는 일, %H는 시간, %M은 분
print(os.date("%Y-%m-%d %H:%M"))  -- 예: 2022-03-03 14:02
```

보다 정교한 날짜 및 시간 조작을 위해 Lua에는 다른 프로그래밍 언어만큼 풍부한 내장 라이브러리가 없습니다. 그러나 `lua-date`(https://github.com/Tieske/date)와 같은 타사 라이브러리를 사용할 수 있습니다. 이 라이브러리는 날짜와 시간을 조작하기 위한 보다 포괄적인 기능을 제공합니다. 다음은 사용 방법입니다:

먼저, `lua-date` 라이브러리가 설치되어 있는지 확인합니다. 주로 LuaRocks를 사용하여 다음 명령어로 설치할 수 있습니다:

```bash
luarocks install lua-date
```

그런 다음, Lua 스크립트에서 다음과 같이 사용할 수 있습니다:

```lua
local date = require("date")

-- 현재 날짜와 시간에 대한 날짜 객체 생성
local now = date()

print(now:fmt("%Y-%m-%d %H:%M:%S"))  -- 예: 2022-03-03 14:02:03
```

이 예제는 `os.date` 함수와 유사하게 포맷할 수 있으면서도 `lua-date` 라이브러리에서 제공하는 추가적인 유연성과 옵션을 사용해 현재 순간을 나타내는 `date` 객체의 생성을 보여줍니다.
