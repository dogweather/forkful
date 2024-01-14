---
title:                "Fish Shell: 날짜를 문자열로 변환하기"
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 왜

날짜를 문자열로 변환하는 것에 대한 이유는 데이터를 다루거나 시간 관련 작업을 할 때 편리하기 때문입니다.

## 어떻게

날짜를 문자열로 변환하는 방법은 간단합니다. 우선 `date` 명령어를 사용하여 현재 날짜와 시간을 확인합니다.

```Fish Shell

> date
Wed Oct 13 14:30:00 KST 2021

```

다음으로 `string` 명령어를 사용하여 날짜를 원하는 형식으로 변환할 수 있습니다. 예를 들어, `+%Y-%m-%d`를 사용하면 년-월-일 형식으로 날짜를 반환할 수 있습니다.

```Fish Shell

> date +%Y-%m-%d
2021-10-13

```

## 깊이 파고들기

일반적으로 날짜 포맷을 변환하기 위해 `date`와 `string` 명령어를 사용하지만, Fish Shell에는 `strftime` 함수를 사용할 수도 있습니다. 이 함수를 사용하면 더 다양한 날짜 포맷을 지정할 수 있습니다. 예를 들어, `%b %d, %Y`를 사용하면 축약된 월 이름, 일, 년 형식으로 날짜를 반환할 수 있습니다.

```Fish Shell

> set date (strftime "%b %d, %Y" (date))
Oct 13, 2021

```

이 외에도 `strftime` 함수에는 많은 옵션이 있으므로 이를 적절히 활용하면 원하는 날짜 포맷을 얻을 수 있습니다.

## 관련 정보보기

- [Fish Shell 사용자 매뉴얼](https://fishshell.com/docs/current/)
- [Shell scripting in Fish Shell](https://blog.sleeplessbeastie.eu/2017/04/11/how-to-script-in-fish/)