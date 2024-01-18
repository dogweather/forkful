---
title:                "문자열에서 날짜 분석하기"
html_title:           "Fish Shell: 문자열에서 날짜 분석하기"
simple_title:         "문자열에서 날짜 분석하기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 무엇이고, 왜 하는가? 
날짜를 문자열에서 추출한다는 건 무엇인지 알 필요가 있습니다. 프로그래머들은 날짜 정보를 데이터베이스에 저장하거나 날짜 필터링을 위해 문자열로부터 식별하기 위해 이 작업을 합니다.

## 해보는 법: 
Fish Shell에서 날짜를 문자열로부터 추출하는 예제와 결과를 살펴보세요. 

```Fish Shell
set date (date -u -f "%Y-%m-%d" "2020-10-31") # 문자열에서 날짜 추출
echo $date # 결과: 2020-10-31T00:00:00Z
```

## 깊게 파보기: 
이 작업에 대해 자세한 정보를 알고 싶다면, 여기 몇 가지 정보를 알려드리겠습니다. 처음부터 날짜를 문자열로부터 추출하기 위한 방법은 비슷하거나 달라질 수 있지만, 대부분의 프로그래머들은 이와 같은 기본적인 형식으로 작업합니다. 비슷한 작업은 다양한 프로그래밍 언어에서도 가능하며, 어떤 언어를 사용하더라도 비슷한 결과를 얻을 수 있습니다. 추출 작업은 날짜가 포함된 문자열에서 날짜를 식별하는 것으로, 이 작업을 수행하기 위해 정규식을 사용하는 경우도 있습니다.

## 관련 자료: 
이 외에도 관련된 정보를 얻고 싶다면, 이 링크들을 참고해보세요. 
- [Fish Shell 공식 홈페이지](https://fishshell.com/)
- [정규식을 사용해서 문자열에서 날짜를 추출하는 방법](https://www.regular-expressions.info/dates.html)
- [Python 프로그래밍 언어를 이용한 날짜 추출 예제](https://www.tutorialspoint.com/python/time_strptime.htm)