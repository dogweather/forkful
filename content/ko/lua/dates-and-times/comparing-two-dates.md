---
date: 2024-01-20 17:33:52.840619-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C \uD560\uAE4C?) Lua\uC758 `os.time()` \uD568\
  \uC218\uB294 \uC8FC\uC5B4\uC9C4 \uB0A0\uC9DC\uC5D0 \uB300\uD55C \uC720\uB2C9\uC2A4\
  \ \uC2DC\uAC01(1970\uB144 1\uC6D4 1\uC77C \uC774\uD6C4 \uCD08 \uB2E8\uC704)\uC744\
  \ \uBC18\uD658\uD569\uB2C8\uB2E4. \uC774\uB294 \uB0A0\uC9DC \uBE44\uAD50\uB97C \uC22B\
  \uC790 \uBE44\uAD50\uCC98\uB7FC \uAC04\uB2E8\uD788 \uD560 \uC218 \uC788\uAC8C \uD574\
  \ \uC90D\uB2C8\uB2E4. \uB300\uC548\uC73C\uB85C, `os.date()`\uB97C \uC0AC\uC6A9\uD574\
  \ \uB354 \uB9CE\uC740 \uB0A0\uC9DC\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:09.733679-06:00'
model: gpt-4-1106-preview
summary: "(\uC5B4\uB5BB\uAC8C \uD560\uAE4C?) Lua\uC758 `os.time()` \uD568\uC218\uB294\
  \ \uC8FC\uC5B4\uC9C4 \uB0A0\uC9DC\uC5D0 \uB300\uD55C \uC720\uB2C9\uC2A4 \uC2DC\uAC01\
  (1970\uB144 1\uC6D4 1\uC77C \uC774\uD6C4 \uCD08 \uB2E8\uC704)\uC744 \uBC18\uD658\
  \uD569\uB2C8\uB2E4."
title: "\uB450 \uB0A0\uC9DC \uBE44\uAD50\uD558\uAE30"
weight: 27
---

## How to: (어떻게 할까?)
```Lua
-- 날짜를 표현하는 두 변수 설정
local date1 = os.time{year=2023, month=4, day=15}
local date2 = os.time{year=2023, month=4, day=18}

-- 날짜 비교
if date1 > date2 then
    print("date1이 date2 이후입니다.")
elseif date1 < date2 then
    print("date1이 date2 이전입니다.")
else
    print("date1과 date2는 동일한 날짜입니다.")
end
```
출력:
```
date1이 date2 이전입니다.
```

## Deep Dive (심층 분석)
Lua의 `os.time()` 함수는 주어진 날짜에 대한 유닉스 시각(1970년 1월 1일 이후 초 단위)을 반환합니다. 이는 날짜 비교를 숫자 비교처럼 간단히 할 수 있게 해 줍니다.

대안으로, `os.date()`를 사용해 더 많은 날짜 정보를 얻고 비교하는 방법도 있습니다. 하지만 대체적으로 두 날짜의 초 단위 표현을 직접 비교하는 것이 간단합니다.

어떤 프로그래밍 언어에서는 복잡한 날짜 처리를 위한 별도의 라이브러리를 제공하지만 Lua에서는 내부 라이브러리가 비교적 단순하기 때문에, 복잡한 날짜 조작이 필요할 경우 외부 라이브러리를 찾아보아야 합니다.

## See Also (관련 자료)
- Lua 5.4 참조 매뉴얼: [os.time](https://www.lua.org/manual/5.4/manual.html#pdf-os.time)
- Lua 5.4 참조 매뉴얼: [os.date](https://www.lua.org/manual/5.4/manual.html#pdf-os.date)
