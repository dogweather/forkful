---
date: 2024-01-20 17:33:17.596329-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) \uB0A0\uC9DC \uBE44\
  \uAD50\uB294 \uC720\uB2C9\uC2A4 \uD0C0\uC784(epoch time)\uC744 \uC0AC\uC6A9\uD558\
  \uC5EC \uC2DC\uC791\uD588\uC2B5\uB2C8\uB2E4. 1970\uB144 1\uC6D4 1\uC77C\uBD80\uD130\
  \ \uC2DC\uC791\uD558\uB294 \uCD08\uC758 \uB204\uC801\uC73C\uB85C, \uC77C\uAD00\uB41C\
  \ \uC2DC\uAC04 \uCE21\uC815 \uBC29\uBC95\uC744 \uC81C\uACF5\uD569\uB2C8\uB2E4. Fish\
  \ Shell\uC758 `date` \uBA85\uB839\uC5B4\uB294 \uC774\uB97C \uD65C\uC6A9\uD558\uC5EC\
  \ \uB0A0\uC9DC \uBE44\uAD50\uB97C \uAC04\uB2E8\uD558\uAC8C\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:10.076610-06:00'
model: gpt-4-1106-preview
summary: "(\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) \uB0A0\uC9DC \uBE44\uAD50\uB294\
  \ \uC720\uB2C9\uC2A4 \uD0C0\uC784(epoch time)\uC744 \uC0AC\uC6A9\uD558\uC5EC \uC2DC\
  \uC791\uD588\uC2B5\uB2C8\uB2E4."
title: "\uB450 \uB0A0\uC9DC \uBE44\uAD50\uD558\uAE30"
weight: 27
---

## How to: (어떻게 하나요?)
```Fish Shell
# 날짜 포맷: YYYY-MM-DD
set date1 "2023-04-01"
set date2 "2023-04-15"

# 날짜를 초(second)로 변환함
set epoch1 (date -ud $date1 +%s)
set epoch2 (date -ud $date2 +%s)

# 초로 표현된 두 날짜 비교
if test $epoch1 -lt $epoch2
    echo "date1 is before date2"
else if test $epoch1 -eq $epoch2
    echo "date1 is the same as date2"
else
    echo "date1 is after date2"
end
```
```
date1 is before date2
```

## Deep Dive (심층 분석)
날짜 비교는 유닉스 타임(epoch time)을 사용하여 시작했습니다. 1970년 1월 1일부터 시작하는 초의 누적으로, 일관된 시간 측정 방법을 제공합니다. Fish Shell의 `date` 명령어는 이를 활용하여 날짜 비교를 간단하게 해줍니다.

대안으로는 Fish Shell 직접 내장 기능을 사용하지 않고, `diff` 같은 다른 프로그램을 사용할 수도 있습니다. 하지만 이 방법은 더 복잡하고, 추가적인 의존성이 필요합니다.

Fish Shell에서 날짜를 처리할 때 주의해야 할 점은 타임존(time zone) 처리입니다. 표준 UTC를 사용하여 날짜가 서로 다른 타임존에서 비교되는 경우의 혼동을 방지할 수 있습니다. 

또한, Fish Shell의 `(date)` 부분에서 다양한 포맷의 날짜를 정확하게 인식하게 만드는 부분이 중요합니다. `%s` 옵션을 통해 epoch 시간으로 변환하고, 이를 비교 기준으로 삼는 것이 일반적인 방식입니다.

## See Also (관련 자료)
- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [Unix Time on Wikipedia](https://en.wikipedia.org/wiki/Unix_time)
- [GNU Coreutils Date](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
