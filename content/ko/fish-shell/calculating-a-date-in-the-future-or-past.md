---
title:                "미래나 과거의 날짜 계산"
html_title:           "Fish Shell: 미래나 과거의 날짜 계산"
simple_title:         "미래나 과거의 날짜 계산"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Fish Shell 프로그래밍: 날짜 계산하기

## 무엇인가요?

날짜 계산은 미래나 과거에서 특정한 날짜를 계산하는 것을 말합니다. 예를 들어, 오늘에서 며칠 후의 날짜나 몇 주 전의 날짜 등을 계산하는 것이 있습니다. 이 작업은 프로그래머들이 자주 사용하는 기능 중 하나입니다.

## 왜 그렇게 하나요?

프로그래머들은 날짜 계산을 수행하여 미래나 과거에서 특정 날짜를 쉽게 파악할 수 있기 때문입니다. 이를 통해 일정 관리나 시간 계획 등의 작업을 효율적으로 수행할 수 있습니다.

## 방법:

날짜 계산을 위해서는 다음과 같이 쉘에서 코드를 입력해야 합니다.

```Fish Shell을 사용할 때:
date 를 사용하여 현재 날짜를 확인합니다.
ex) date +'%Y-%m-%d' #오늘의 날짜를 년-월-일로 표시합니다.
따라서 다음과 같이 미래 또는 과거 날짜를 계산할 수 있습니다.

```Fish Shell을 사용할 때:
date -d '2 days' +'%Y-%m-%d' #오늘 기준 2일 후의 날짜를 년-월-일로 표시합니다.
```

위와 같은 방법으로 다양한 날짜 계산을 할 수 있습니다.

## 깊이 파보기:

날짜 계산은 오래된 컴퓨터 프로그래밍에서 이미 사용되고 있습니다. 예를 들어, 1970년 1월 1일을 기준점으로 잡아 이전 날짜는 음수, 이후 날짜는 양수로 계산하는 방식이 있었습니다. 이 때문에 시간 표현의 문제로 오류가 발생하기도 했습니다.

하지만 최근에는 날짜 계산을 위한 전용 라이브러리나 프로그램들이 개발되어 좀 더 정확한 계산이 가능해졌습니다. 이외에도 다양한 알고리즘과 방식을 이용하여 날짜 계산을 하는 방법도 있습니다.

## 관련 자료:

다른 라이브러리나 프로그램을 이용하여 날짜 계산을 해보고 싶으시다면 다음 링크를 참고해보세요.

- [Moment.js](https://momentjs.com/) : JavaScript에서 날짜와 시간 관련 작업을 쉽게 처리할 수 있는 라이브러리
- [Dateutil](https://dateutil.readthedocs.io/en/stable/) : 파이썬에서 날짜와 시간 관련 작업을 처리하는 라이브러리
- [Calendar API](https://www.w3schools.com/Jsref/jsref_obj_date.asp) : JavaScript에서 날짜 계산을 위한 내장된 함수들을 제공하는 API