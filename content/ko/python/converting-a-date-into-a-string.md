---
title:    "Python: 날짜를 문자열로 변환하기"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 왜
날짜를 문자열로 변환하는 것에는 어떤 이유가 있을까요? 예를 들어, 날짜 데이터를 사용자가 이해할 수 있는 형식으로 출력하기 위해서일 수 있습니다.

## 방법
```Python
# datetime 모듈 불러오기
import datetime

# 현재 시간 불러오기
now = datetime.datetime.now()

# 문자열로 변환하기
now_string = now.strftime("%Y년 %m월 %d일 %H시 %M분")

# 출력
print(now_string)

# 결과
2021년 10월 18일 15시 30분
```

## 깊이 파고들기
날짜를 문자열로 변환하는 방법은 datetime 모듈의 strftime() 메소드를 사용하는 것입니다. 이 메소드는 특정 형식의 문자열을 반환하는데, %Y는 연도, %m은 월, %d는 일, %H는 시간, %M은 분을 나타내는 기호입니다. 이 기호들을 조합하여 원하는 형식의 날짜 문자열을 만들 수 있습니다.

## 참고하기
https://docs.python.org/3/library/datetime.html  - datetime 모듈 문서
https://strftime.org/  - strftime() 메소드에 사용되는 기호들의 목록