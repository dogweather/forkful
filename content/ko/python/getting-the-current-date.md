---
title:    "Python: 현재 날짜 받아오기"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

# 왜

일상의 많은 일들 중 하나는 현재 날짜를 알아내는 것입니다. 우리는 주로 일정을 계획하고 다가오는 이벤트나 기념일을 조직하기 위해 현재 날짜를 확인하며 시간을 관리하기도 합니다. 이러한 이유로 매우 중요한 일입니다. 이제 우리는 파이썬을 사용하여 현재 날짜를 얻는 방법에 대해 알아보겠습니다.

# 어떻게 하나요

가장 간단한 방법은 파이썬의 내장 모듈인 `datetime`을 사용하는 것입니다. 다음의 코드를 실행해보세요.

```Python
from datetime import datetime
now = datetime.now()
print(now)
```

출력은 다음과 비슷할 것입니다.

```
2021-05-05 14:00:00
```

여기서 날짜와 시간이 년-월-일 시:분:초 형식으로 표시됩니다. 만약 당신이 원하는 형식이 다르다면 `strftime()` 메소드를 사용하여 출력 형식을 조정할 수 있습니다. 예를 들면 다음과 같습니다.

```Python
from datetime import datetime
now = datetime.now()
formatted = now.strftime("%Y년 %m월 %d일 %H시%M분%S초")
print(formatted)
```

출력은 다음과 같이 변경될 것입니다.

```
2021년 05월 05일 14시00분00초
```

# 깊이 파고들기

파이썬의 `datetime` 모듈은 매우 강력하고 다양한 메소드를 제공합니다. 주어진 일시에서 특정 요소(년, 월, 일, 시간 등)를 얻을 수 있습니다. 또한 두 날짜 간의 차이를 계산하거나 시간을 조작하는 데 사용할 수 있는 다양한 함수도 제공됩니다.

예를 들어, `now` 변수의 값에서 현재 시간을 알아내려면 `hour` 속성을 사용하시면 됩니다.

```Python
print(now.hour)
```

출력은 현재 시간을 24시간 형식으로 나타낼 것입니다.

```
14
```

더 많은 정보는 [파이썬 공식 문서](https://docs.python.org/3/library/datetime.html)를 참조해주세요.

# 같이 보기

- [Python 공식 문서](https://www.python.org/)
- [Python Tutorial (한국어)](https://docs.python.org/ko/3/tutorial/index.html)
- [Python 한국 사용자 그룹 (한국어)](https://www.facebook.com/groups/pythonkorea/)