---
title:                "문자열에서 날짜 분석하기"
html_title:           "Python: 문자열에서 날짜 분석하기"
simple_title:         "문자열에서 날짜 분석하기"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

문자열에서 날짜를 추출하는것은 파이썬 프로그래머들이 자주 하는 작업입니다. 일반적으로, 날짜를 포함하는 문자열을 읽어들이고, 이를 컴퓨터가 이해할 수 있는 형태로 변환하는 것입니다. 날짜 데이터는 많은 프로그램에서 중요한 역할을 하기 때문에, 정확하게 분석하여 추출하는 것이 중요합니다.

## 방법:

날짜를 추출하는 가장 간단한 방법은 문자열 포맷을 이용하는 것입니다. 예를 들어, "2020년 10월 10일"이라는 문자열을 파싱하여 날짜 객체로 변환하려면 다음과 같이 코드를 작성할 수 있습니다.

```python
date_string = "2020년 10월 10일"
date_object = datetime.strptime(date_string, '%Y년 %m월 %d일')
print(date_object)
```

출력결과는 `2020-10-10 00:00:00`일 것입니다.

## 깊게 파헤치기:

날짜를 추출하는 것은 날짜 포맷이 중요한 이유 때문입니다. 예를 들어, "10/10/2020"이라는 문자열에서는 일/월/년 순서로 날짜가 표시됩니다. 하지만 다른 지역의 문자열에서는 월/일/년 순서로 표시될 수 있습니다. 따라서 프로그래머는 어떤 지역에서 사용되는 날짜 포맷인지 파악하여 그에 맞춰 문자열을 파싱해야 합니다.

또한, 날짜를 추출하는 다른 방법으로는 정규표현식을 활용하는 것이 있습니다. 하지만 이 방법은 문자열이 일정한 패턴을 가지고 있을 때만 사용할 수 있기 때문에 일반적인 방법보다 조금 더 복잡합니다.

## 관련 문서:

- [날짜 데이터 포맷 가이드](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-format-codes)
- [정규표현식 파이썬 라이브러리](https://docs.python.org/3/library/re.html)