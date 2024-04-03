---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:14.192656-07:00
description: "\uC0AC\uC6A9 \uBC29\uBC95: Lua\uB294 \uD604\uC7AC \uB0A0\uC9DC\uC640\
  \ \uC2DC\uAC04\uC744 \uC5BB\uAE30 \uC704\uD574 `os.date` \uD568\uC218\uB97C \uC81C\
  \uACF5\uD569\uB2C8\uB2E4. \uC774 \uD568\uC218\uB294 \uD615\uC2DD\uD654\uB41C \uBB38\
  \uC790\uC5F4\uC744 \uC5BB\uAE30 \uC704\uD574 \uC544\uADDC\uBA3C\uD2B8 \uC5C6\uC774\
  \ \uC0AC\uC6A9\uD560 \uC218 \uC788\uAC70\uB098 \uCD9C\uB825\uC744 \uC0AC\uC6A9\uC790\
  \ \uC815\uC758\uD558\uAE30 \uC704\uD574 \uD615\uC2DD \uC9C0\uC815\uC790\uC640 \uD568\
  \uAED8 \uC0AC\uC6A9\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4. \uC0AC\uC6A9 \uBC29\uBC95\
  \uC740 \uB2E4\uC74C\uACFC \uAC19\uC2B5\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.432675-06:00'
model: gpt-4-0125-preview
summary: "Lua\uB294 \uD604\uC7AC \uB0A0\uC9DC\uC640 \uC2DC\uAC04\uC744 \uC5BB\uAE30\
  \ \uC704\uD574 `os.date` \uD568\uC218\uB97C \uC81C\uACF5\uD569\uB2C8\uB2E4."
title: "\uD604\uC7AC \uB0A0\uC9DC \uAC00\uC838\uC624\uAE30"
weight: 29
---

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
