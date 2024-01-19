---
title:                "두 날짜 비교하기"
html_title:           "C#: 두 날짜 비교하기"
simple_title:         "두 날짜 비교하기"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 무엇 & 왜? (What & Why?)

두 날짜를 비교한다는 것은 날짜 간의 차이를 찾는 것입니다. 이를 통해 프로그래머는 일정 순서의 이벤트를 정렬하거나 특정 날짜 간의 기간을 계산할 수 있습니다.

## 방법 (How to)

Lua에서는 os 모듈의 `os.time` 함수를 사용하여 날짜를 비교합니다.

```Lua
date1 = os.time{year=2022, month=7, day=20}
date2 = os.time{year=2022, month=11, day=30}

if date1 < date2 then
  print("The first date is earlier than the second.")
else
  print("The first date is later than the second.")
end
```

출력 결과:

```
The first date is earlier than the second.
```

## 심층 탐구 (Deep Dive)

Lua에서는 날짜를 클래스에 저장할 수 있기 때문에 복잡한 날짜 연산을 수행할 수 있습니다. 이러한 방법은 time 클래스를 사용하는 C 같은 다른 언어에서도 볼 수 있습니다.

대안으로, 다른 날짜 함수를 사용할 수도 있습니다. 예를 들면, 두 날짜 간의 차이를 초 단위로 반환하는 `os.difftime` 함수 등이 있습니다.

Lua의 날짜 비교 구현은 초 단위의 타임 스탬프를 이용합니다. 이로 인해, 범위가 좁아진다는 문제를 갖고 있지만, 실제로 그보다 더 커진 문제를 만나는 경우는 드뭅니다.

## 참고 자료 (See Also)

- Lua os 모듈 문서: [os.time, os.difftime](https://www.lua.org/manual/5.4/manual.html#6.9)
- 날짜와 시간에 관한 Lua 튜토리얼: [Lua Date & Time](https://www.tutorialspoint.com/lua/lua_date_time.htm)