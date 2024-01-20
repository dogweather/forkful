---
title:                "미래나 과거의 날짜 계산하기"
html_title:           "Fish Shell: 미래나 과거의 날짜 계산하기"
simple_title:         "미래나 과거의 날짜 계산하기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

---

## 무엇이고 왜 필요할까?

날짜 계산은 미래 또는 과거의 특정 날짜를 찾아내는 것입니다. 이 기능은 특정 이벤트가 발생할 날짜를 예측하거나, 일정 기간 후의 날짜를 구할 때 등 프로그래머들이 자주 사용합니다.

## 어떻게 하나요?

아래는 Fish Shell 에서 미래의 날짜를 어떻게 계산하는지 설명하는 코드입니다.

```bash
# 3일 뒤의 날짜를 찾기
set future_date (date --date="+3 day" +'%Y-%m-%d')

# 출력
echo $future_date
```
이 코드는 실행하면 현재 날짜로부터 3일 후의 날짜를 출력합니다. 주어진 날짜는 형식은 '연도-월-일' 입니다.

## 깊게 알아보기

먼저, Unix 에서의 날짜 계산은 1970년 1월 1일(UTC) 이후로 경과한 시간을 계산합니다. 이 때문에 이는 'epoch time' 이라고도 합니다.

다음으로, Fish Shell 이외에도 Python, Java 등 다른 언어들에서도 날짜 계산 기능을 제공하고 있습니다. 이들은 본인만의 내장 라이브러리를 가지고 있기 때문에 Fish Shell 보다 더 다양한 기능을 제공하고 있는 경우도 많습니다.

마지막으로, 날짜 계산에 있어서 구현 상의 세부사항들을 이해하는 것은 매우 중요합니다. 예를 들면, UTC 이외의 시간대를 고려해야 하거나, 윤년을 고려해야 하는 경우도 있습니다.

## 함께 보기

이와 관련한 추가 자료들은 아래 링크에서 확인하실 수 있습니다.

- Fish Shell 공식 문서: <https://fishshell.com/docs/current/>
- 날짜 시간 계산에 대한 좋은 자료: <https://www.epochconverter.com/>
- Python 날짜 계산법: <https://docs.python.org/3/library/datetime.html>

---