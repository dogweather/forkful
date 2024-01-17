---
title:                "미래나 과거의 날짜 계산하기"
html_title:           "Gleam: 미래나 과거의 날짜 계산하기"
simple_title:         "미래나 과거의 날짜 계산하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# 너의 정확한 미래의 일자를 계산하는 방법

## 무엇과 왜?

날짜를 미래나 과거로 계산하는 것은 프로그래머들이 미래 일자에 대해 새로운 계획을 세우기 위해 필요한 일입니다. 예를 들어, 프로젝트의 마감일이 내일인데 그 전날까지 고칠 부분이 남아 있을 경우, 미래 일자 계산 기능을 사용하여 마감일을 재조정할 수 있습니다.

## 하는 법:

Gleam에서는 `Calendar`모듈을 사용하여 미래 또는 과거 일자를 계산할 수 있습니다. ```Gleam
let day = Calendar.Day.from_gregorian_date(2021, 11, 15)
let two_days_after = Calendar.add_days(day, 2)
``` 
위의 예시에서는 2021년 11월 15일을 변수 `day`에 넣고, `Calendar.add_days` 함수를 사용해 2일 후의 일자를 계산해 변수 `two_days_after`에 저장합니다. 코드를 실행하면 `two_days_after`에는 `2021, 11, 17`이라는 값이 저장됩니다.

## 더 자세히:

미래나 과거 일자를 계산하는 기능은 현재까지 많은 프로그래밍 언어에서 사용되었습니다. 그 중에서도 가장 널리 쓰이는 것은 Ruby의 `Date` 모듈입니다. 하지만 Ruby의 `Date` 모듈은 오로지 그레고리력만을 지원하며, Gleam의 `Calendar` 모듈은 다양한 달력을 지원합니다.

## 더 알아보기:

- [Gleam 공식 문서 - Calendar 모듈](https://gleam.run/documentation/standard-library/calendar/)
- [Ruby 공식 문서 - Date 모듈](https://ruby-doc.org/stdlib-3.0.0/libdoc/date/rdoc/Date.html)