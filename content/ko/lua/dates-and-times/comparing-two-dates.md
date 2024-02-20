---
date: 2024-01-20 17:33:52.840619-07:00
description: "\uB450 \uB0A0\uC9DC\uB97C \uBE44\uAD50\uD55C\uB2E4\uB294 \uAC83\uC740\
  \ \uAC04\uB2E8\uD558\uAC8C \uC5B4\uB290 \uB0A0\uC9DC\uAC00 \uC774\uC804\uC774\uAC70\
  \uB098 \uC774\uD6C4\uC778\uC9C0, \uD639\uC740 \uB3D9\uC77C\uD55C\uC9C0 \uACB0\uC815\
  \uD558\uB294 \uACFC\uC815\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\
  \uC740 \uAE30\uAC04\uC744 \uACC4\uC0B0\uD558\uAC70\uB098 \uC774\uBCA4\uD2B8 \uC21C\
  \uC11C\uB97C \uC815\uD558\uB294 \uB4F1\uC758 \uC791\uC5C5\uC744 \uC704\uD574 \uC774\
  \uB97C \uC218\uD589\uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: 2024-02-19 22:05:14.347872
model: gpt-4-1106-preview
summary: "\uB450 \uB0A0\uC9DC\uB97C \uBE44\uAD50\uD55C\uB2E4\uB294 \uAC83\uC740 \uAC04\
  \uB2E8\uD558\uAC8C \uC5B4\uB290 \uB0A0\uC9DC\uAC00 \uC774\uC804\uC774\uAC70\uB098\
  \ \uC774\uD6C4\uC778\uC9C0, \uD639\uC740 \uB3D9\uC77C\uD55C\uC9C0 \uACB0\uC815\uD558\
  \uB294 \uACFC\uC815\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740\
  \ \uAE30\uAC04\uC744 \uACC4\uC0B0\uD558\uAC70\uB098 \uC774\uBCA4\uD2B8 \uC21C\uC11C\
  \uB97C \uC815\uD558\uB294 \uB4F1\uC758 \uC791\uC5C5\uC744 \uC704\uD574 \uC774\uB97C\
  \ \uC218\uD589\uD569\uB2C8\uB2E4."
title: "\uB450 \uB0A0\uC9DC \uBE44\uAD50\uD558\uAE30"
---

{{< edit_this_page >}}

## What & Why? (무엇을, 왜?)
두 날짜를 비교한다는 것은 간단하게 어느 날짜가 이전이거나 이후인지, 혹은 동일한지 결정하는 과정입니다. 프로그래머들은 기간을 계산하거나 이벤트 순서를 정하는 등의 작업을 위해 이를 수행합니다.

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
