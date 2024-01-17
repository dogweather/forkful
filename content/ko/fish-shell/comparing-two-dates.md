---
title:                "두 날짜 비교하기"
html_title:           "Fish Shell: 두 날짜 비교하기"
simple_title:         "두 날짜 비교하기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

# 무엇 & 왜?
두 날짜를 비교하는 것은 일반적으로 프로그래머가 날짜/시간 데이터를 다룰 때 필요합니다. 예를 들어, 두 날짜가 같은지 다른지를 판별하거나, 지난 날짜와 현재 날짜를 비교하여 얼마나 시간이 지났는지를 계산할 수 있습니다.

# 방법:
두 날짜를 비교하는 것은 Fish Shell에서 아주 쉽게 할 수 있습니다. 다음과 같이 입력해 보세요:

```
set start (date -r 1479254717)
set end (date -r 1510790717)

if [ $start = $end ]
    echo "The dates are the same."
else if [ $start -lt $end ]
    echo "The second date is after the first date."
else
    echo "The first date is after the second date."
```

위의 코드를 실행하면 두 날짜가 같은 지 체크하고, 두 번째 날짜가 첫 번째 날짜보다 며칠 뒤인지, 그리고 첫 번째 날짜가 두 번째 날짜보다 며칠 뒤인지를 알려줍니다. 결과는 다음과 같이 나옵니다:

```
The second date is after the first date.
```

# 깊게 들어가기:
날짜를 비교하는 방법은 실제로 프로그래밍 언어마다 조금씩 다를 수 있지만, 일반적인 방법은 날짜를 숫자로 치환하여 비교하는 것입니다. Fish Shell에서는 `date` 명령어를 통해 비교할 수 있게 됩니다. 또 다른 옵션으로는 `cmp`라는 명령어도 있습니다. 하지만 `date` 명령어가 더 보기 쉽고 사용하기 간편하기 때문에, 일반적으로 더 많이 사용됩니다.

# 관련 문서:
- Fish Shell의 `date` 명령어 문서: https://fishshell.com/docs/current/cmds/date.html
- `cmp` 명령어에 대한 Stack Overflow 답변: https://stackoverflow.com/questions/32812148/how-to-compare-date-in-fish-shell-script