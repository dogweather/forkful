---
title:                "Python: 두 날짜 비교하기"
programming_language: "Python"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/comparing-two-dates.md"
---

{{< edit_this_page >}}

# 왜?

두 날짜를 비교하는것에 참여할 이유는 무엇일까요? Python 프로그래밍에서 날짜 비교가 왜 필요한지 이해하고 싶을 수 있습니다. 두 날짜를 비교해야 하는 이유는 여러가지 있을 수 있지만, 가장 일반적인 이유는 특정 날짜를 기준으로 다른 날짜가 더 늦거나 이를 기록하는 것입니다. 이를 통해 이벤트의 발생 순서를 파악하거나 지난 날짜와 최신 날짜의 차이를 계산할 수 있습니다. 

# 방법

Python에서 두 날짜를 비교하는 방법은 다양하지만, 가장 간단한 방법은 datetime 모듈을 사용하는 것입니다. 다음은 두 개의 날짜를 비교하는 예시 코드입니다.

```Python
from datetime import datetime

# 두 날짜를 변수에 할당
date1 = datetime(2021, 5, 5)
date2 = datetime(2021, 5, 10)

# 비교 연산자를 사용하여 두 날짜를 비교
if date1 > date2:
    print("date1이 더 늦은 날짜입니다.")
elif date1 < date2:
    print("date2가 더 늦은 날짜입니다.")
else:
    print("두 날짜는 같은 날짜입니다.")
    
# Output: date2가 더 늦은 날짜입니다.
```

위 예시 코드에서는 두 날짜를 비교하여 더 늦은 날짜를 출력하는 간단한 비교 연산을 수행합니다. datetime 모듈에는 여러 다른 함수와 비교 연산이 있으므로, 찾아보고 싶은 경우 공식 문서를 확인해보세요.

# 딥 다이브

더 깊이 들어가서 두 날짜를 비교하는 방법에 대해 알아보겠습니다. 날짜를 비교하는 경우에는 날짜와 시간을 포함하여 비교해야 할 수도 있습니다. 이 경우, 비교 연산자만으로는 충분하지 않고 다른 함수를 사용해야 할 수 있습니다. 또한, 날짜를 비교할 때 우리가 원하는 형식에 따라 코드를 조금 수정해야 할 수도 있습니다. 더 많은 정보를 알고 싶은 경우에는 Python의 날짜 및 시간 관련 철학적 지점을 포함하는 PEP 495를 참조하세요.

# 관련 자료들

## 참고 자료

- [Python datetime 모듈 문서](https://docs.python.org/ko/3/library/datetime.html)
- [Python 날짜 비교 예제 코드](https://www.tutorialspoint.com/compare-two-dates-in-python)
- [PEP 495 - Python 3.6 문서](https://www.python.org/dev/peps/pep-0495/)

## 추가 자료

- [Python datetime 모듈의 튜토리얼](https://realpython.com/python-datetime/)
- [Python 날짜 및 시간 관련 팁과 트릭](https://towardsdatascience.com/work-with-dates-like-a-pro-in-python-8ff19878e9d9)
- [Python datetime 모듈에서 날짜 비교하기](https://www.geeksforgeeks.org/python-compare-two-dates/)