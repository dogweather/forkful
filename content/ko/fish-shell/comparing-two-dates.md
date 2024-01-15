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

## 왜

두 날짜를 비교하는 것은 일정한 프로젝트를 추적하고 분석하기 위해 필요한 중요한 과정입니다.

## 방법

두 날짜를 비교하는 가장 간단한 방법은 `date` 명령어를 사용하는 것입니다. 다음은 두 날짜를 비교하는 예제 코드입니다:

```Fish Shell
set today (date +%Y-%m-%d)
set tomorrow (date -d "1 day" +%Y-%m-%d)

if [ $today \< $tomorrow ]
        echo "Today is earlier than tomorrow!"
end
```

위 코드를 실행하면 "Today is earlier than tomorrow!" 라는 결과가 출력됩니다. 또한 `=`(같다), `>`(나중), `>=`(나중이거나 같음), `<`(이전), `<=`(이전이거나 같음) 연산자를 사용하여 두 날짜를 비교할 수도 있습니다.

## 깊이 파고들기

날짜를 비교할 때 유의해야 할 점은 날짜 형식이 일치해야 한다는 것입니다. 예를 들어, `%Y-%m-%d` 형식으로 저장된 날짜를 비교할 때에는 두 날짜가 동일한 형식으로 입력되어야 합니다. 또한, 년도, 월, 일 순서로 비교하게 됩니다. 따라서 년도가 먼저 비교되고 같은 경우에만 월을 비교하고 그 다음에 일을 비교하게 됩니다.

## 더 찾아보기

- 실험을 위해 두 날짜를 비교하는 다른 방법: https://fishshell.com/docs/current/cmds/date.html
- 날짜 형식 목록: https://fishshell.com/docs/current/tutorial.html#tutorial_datetime_format
- Fish Shell 공식 문서: https://fishshell.com/docs/current/index.html#