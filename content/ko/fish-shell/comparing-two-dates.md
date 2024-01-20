---
title:                "두 날짜 비교하기"
html_title:           "C#: 두 날짜 비교하기"
simple_title:         "두 날짜 비교하기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 뭐고 왜요?
날짜 비교는 두 날짜의 차이를 얻기 위해 사용합니다. 프로그래머들은 이를 통해 두 이벤트 간의 시간적 거리를 측정하거나 미래 이벤트가 얼마나 먼지 확인하는데 사용합니다.

## 어떻게 하나요:
Fish Shell에서 날짜를 비교하기 위해 다음과 같은 코드를 사용할 수 있습니다:

```Fish Shell
set date1 (date -d '2021-01-01' +%s)
set date2 (date -d '2021-01-31' +%s)
math $date2 - $date1
```
위 프로그램을 실행하면, 출력은 '2592000'으로 나타납니다. 이는 두 날짜 간의 초 단위 차이를 표시한 것입니다.

## 깊게 알아보기
Fish Shell은 2005년에 첫 릴리스되어, Bash나 Zsh와 비교하여 대체적으로 사용자 친화적입니다. 날짜 비교는 상당히 복잡한 작업일 수 있지만, Fish Shell은 이를 단순화합니다. 
대안으로, 보다 복잡한 시나리오의 경우 Python과 같은 다른 언어를 사용하는 것이 좋을 수 있습니다. Fish Shell에서 날짜 비교는 Unix epoch (1970년 1월 1일)을 초 단위로 표현한 숫자로 작업합니다.

## 참고 자료들
날짜를 비교하는 다른 방법을 알아보려면:
- [Fish Shell 튜토리얼](https://fishshell.com/docs/current/index.html)
- [날짜에 대한 리눅스 문서](https://www.linux.com/topic/desktop/date-command-linux/)
- [Python에서 날짜와 시간을 다루는 방법](https://docs.python.org/3/library/datetime.html)