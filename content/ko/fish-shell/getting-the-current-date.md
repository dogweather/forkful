---
title:                "Fish Shell: 현재 날짜 받기"
programming_language: "Fish Shell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 왜 필요한가? 

현재 날짜를 알고 싶은 이유는 다양할 수 있습니다! 일상적으로는 프로그램을 실행할 때마다 현재 날짜가 필요할 수 있습니다. 또는 날짜를 기반으로 한 다양한 기능을 포함하는 프로그램을 개발하고자 할 때도 마찬가지입니다. 어떤 이유든 간에 현재 날짜를 가져오는 것은 매우 유용합니다!

## 어떻게 하나요? 

Fish Shell을 사용하면 쉽게 현재 날짜를 가져올 수 있습니다. 아래 코드 블록을 참고하여 보시죠!

```Fish Shell
set curr_date (date +%Y-%m-%d)
echo $curr_date
```

위 코드를 실행하면 현재 날짜를 년-월-일 형식으로 출력해주게 됩니다. 예를 들어, 오늘 날짜가 2021년 11월 3일이라면 "2021-11-03"이 출력됩니다. 또한, date 명령어를 사용하면 시간 정보까지 함께 가져올 수 있습니다. 아래 코드를 참고해보세요.

```Fish Shell
set curr_date_time (date +%Y-%m-%d_%H:%M:%S)
echo $curr_date_time
```

위 코드를 실행하면 현재 날짜와 시간을 년-월-일_시:분:초 형식으로 출력해줍니다. 예를 들어, 현재 시간이 오후 2시 30분 10초라면 "2021-11-03_14:30:10"이 출력됩니다.

## 더 깊게 들어가보기 

"date +%Y-%m-%d"와 같은 형식으로 사용하는 date 명령어는 날짜 형식을 원하는 대로 설정할 수 있습니다. 예를 들어, %Y 대신 %d를 사용하면 일만 출력되며, %b를 사용하면 현재 월의 약어가 출력됩니다. 더 자세한 정보는 아래 링크들을 참고해보세요.

## 같이 보기 

- [Fish Shell 공식 문서](https://fishshell.com/docs/current/cmds/date.html)
- [GNU 'date' 명령어 설명서](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)