---
title:                "Fish Shell: 미래나 과거의 날짜 계산하기"
simple_title:         "미래나 과거의 날짜 계산하기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 왜

여러분은 날짜를 미래 혹은 과거로 계산하는 것에 참여하는 이유가 무엇인지 궁금했을 것입니다. 배경지식이 많으면 이것을 해야하는 이유를 깊이 이해할 수도 있습니다. 그러나 대부분 사람들은 단순히 그런 기능이 필요하다고 알고 있을 것입니다. 그리고 다행히도 Fish Shell에서는 쉽게 날짜를 계산할 수 있는 여러 옵션이 있습니다.

## 사용 방법

우선 Fish Shell을 열고 다음과 같이 입력하십시오.

```
Fish Shell을 계산할 방법을 보여주는 코드 블록
```

이렇게 하면 Fish Shell에 내장된 `date` 명령어를 사용할 수 있습니다. `date` 명령어는 사용자가 지정한 형식의 날짜와 시간을 출력해주는 역할을 합니다.

예를 들어, 만약 우리가 오늘의 날짜를 계산한다고 가정하고 싶다면, 다음과 같이 입력할 수 있습니다.

`date '+오늘은 %Y년 %m월 %d일입니다.'`

이렇게 입력하면 다음과 같은 결과를 볼 수 있습니다.

`오늘은 2021년 01월 01일입니다.`

또한 우리는 오늘로부터 1일 후의 날짜를 계산하고 싶다면 다음과 같이 입력할 수 있습니다.

`date -v +1d '+1일 후의 날짜는 %Y년 %m월 %d일입니다.'`

그러면 결과로 다음과 같이 볼 수 있습니다.

`1일 후의 날짜는 2021년 01월 02일입니다.`

## 깊이 판단

방금 살펴본 것과 같이 Fish Shell에서는 `date` 명령어를 사용하여 간단하게 날짜를 계산할 수 있습니다. 하지만 실제로는 날짜를 계산하는 방법이 더 깊이 있습니다. 예를 들어, 날짜를 특정 기간 전으로 계산하는 것도 가능합니다. 또한 사용자가 원하는 날짜 형식을 사용하여 날짜를 계산할 수도 있습니다.

더 깊게 알고 싶다면 Fish Shell 공식 문서를 참조하시거나, 다음의 링크들을 살펴보세요.

## 같이 보기

- [Fish Shell 공식 문서](https://fishshell.com/docs/current/cmds/date.html)
- [Fish Shell로 날짜와 시간 계산하기](https://queensland.decubitus.net/fish-shell-date/)