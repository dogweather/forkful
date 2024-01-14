---
title:    "Python: 현재 날짜 받기"
keywords: ["Python"]
---

{{< edit_this_page >}}

# 왜 

우리는 일상적으로 현재 날짜를 알아야 합니다. 예를 들어, 우리의 일정을 기록하고 몇 일 후에 어떤 일이 예정되어 있는지 파악하기 위해서입니다. 이를 위해서는 파이썬에서 현재 날짜를 얻는 방법을 배우는 것이 중요합니다. 

# 어떻게 하나요? 

우리는 파이썬의 내장 모듈인 'datetime'을 사용하여 현재 날짜를 찾을 수 있습니다. 먼저, 모듈을 불러오고 현재 날짜를 오늘 변수에 저장합니다. 그런 다음, 오늘 변수에서 원하는 형식으로 날짜를 출력할 수 있습니다. 

```Python
import datetime

오늘 = datetime.date.today()
print(오늘)
```

위의 코드를 실행하면 오늘의 날짜가 '연-월-일' 형식으로 출력됩니다. 

# 딥 다이브 

더 많은 날짜 정보를 얻고 싶다면 'time' 모듈을 사용할 수 있습니다. 이를 사용하면 현재 시간까지 얻을 수 있습니다. 먼저 모듈을 불러옵니다. 그리고 마찬가지로 현재 시간을 변수에 저장한 후, 원하는 형식으로 출력할 수 있습니다. 

```Python
import time

현재시간 = time.localtime()
print("현재 시간: ", 현재시간)
print("시: ", 현재시간.tm_hour)
print("분: ", 현재시간.tm_min)
print("초: ", 현재시간.tm_sec)
```

위의 코드를 실행하면 현재 시간과 시, 분, 초를 따로 출력할 수 있습니다. 이를 활용하면 다양한 형식으로 날짜 및 시간을 출력할 수 있습니다. 

# 관련 자료 

- [파이썬 공식 문서](https://docs.python.org/ko/3/library/datetime.html)
- [점프 투 파이썬 - 날짜와 시간 다루기](https://wikidocs.net/26) 
- [W3Schools - Datetime 모듈](https://www.w3schools.com/python/python_datetime.asp) 

# 더 궁금한 점이 있다면 위의 자료를 참고해보세요!