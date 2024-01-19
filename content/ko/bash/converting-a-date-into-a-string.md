---
title:                "날짜를 문자열로 변환하기"
html_title:           "Arduino: 날짜를 문자열로 변환하기"
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 무엇이며 왜하는가?
날짜를 문자열로 바꾸는 것은 특정 날짜를 문자의 형태로 나타내는 과정입니다. 이를 통해 프로그래머는 날짜 정보를 다른 문자열과 함께 쉽게 처리하고 출력할 수 있습니다.

## 어떻게 하는가:
다음은 Bash에서 날짜를 문자열로 변환하는 방법에 대한 예입니다:

```Bash
date_today=`date '+%Y-%m-%d'`
echo $date_today
```

이 코드에서 'date'는 현재 날짜와 시간을 출력하는 Bash 명령입니다. '+%Y-%m-%d'는 출력 형식을 나타냅니다. 이 경우 연(Year)-월(Month)-일(Day) 형식의 문자열로 날짜를 출력합니다.

## 깊이 있게 알아보기
`date` 명령어는 Unix 계열 운영 체제에서 제공하는 기본 명령어 중 하나로, 1970년대 초반에 처음 소개되었습니다. 대안으로는 C 라이브러리의 `strftime()` 함수나 Python의 `datetime` 모듈 등이 있습니다. Bash에서 `date` 명령어를 이용해 날짜를 문자열로 변환하는 방법은 내부적으로 시스템의 시간 라이브러리를 호출하여 시간 정보를 가져온 후, 사용자가 지정한 형식으로 해당 시간 정보를 변환하는 방식으로 이루어집니다.

## 참고 자료
다음은 날짜를 문자열로 변환하는 방법에 대한 추가정보를 제공하는 웹사이트들입니다:
1. [`date`](https://man7.org/linux/man-pages/man1/date.1.html) 명령어에 대한 메뉴얼 페이지
2. C 라이브러리의 [`strftime()`](http://www.cplusplus.com/reference/ctime/strftime/) 함수에 대한 참고서
3. Python의 [`datetime`](https://docs.python.org/3/library/datetime.html) 모듈에 대한 공식 문서