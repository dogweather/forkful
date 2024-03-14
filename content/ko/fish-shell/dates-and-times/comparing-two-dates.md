---
date: 2024-01-20 17:33:17.596329-07:00
description: "\uB450 \uB0A0\uC9DC\uB97C \uBE44\uAD50\uD55C\uB2E4\uB294 \uAC74, \uB450\
  \ \uC2DC\uC810 \uC0AC\uC774\uC758 \uCC28\uC774\uB97C \uC54C\uC544\uB0B4\uB294 \uAC83\
  \uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uAE30\uAC04\uC744\
  \ \uACC4\uC0B0\uD558\uAC70\uB098 \uB0A0\uC9DC\uB97C \uC815\uB82C\uD558\uAE30 \uC704\
  \uD574 \uB0A0\uC9DC\uB97C \uBE44\uAD50\uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.873817-06:00'
model: gpt-4-1106-preview
summary: "\uB450 \uB0A0\uC9DC\uB97C \uBE44\uAD50\uD55C\uB2E4\uB294 \uAC74, \uB450\
  \ \uC2DC\uC810 \uC0AC\uC774\uC758 \uCC28\uC774\uB97C \uC54C\uC544\uB0B4\uB294 \uAC83\
  \uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uAE30\uAC04\uC744\
  \ \uACC4\uC0B0\uD558\uAC70\uB098 \uB0A0\uC9DC\uB97C \uC815\uB82C\uD558\uAE30 \uC704\
  \uD574 \uB0A0\uC9DC\uB97C \uBE44\uAD50\uD569\uB2C8\uB2E4."
title: "\uB450 \uB0A0\uC9DC \uBE44\uAD50\uD558\uAE30"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
두 날짜를 비교한다는 건, 두 시점 사이의 차이를 알아내는 것입니다. 프로그래머들은 기간을 계산하거나 날짜를 정렬하기 위해 날짜를 비교합니다.

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
