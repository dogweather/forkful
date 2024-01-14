---
title:                "Python: 미래나 과거 날짜 계산하기"
programming_language: "Python"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 왜?

일반적으로 우리는 새해에 어떤 날짜로부터 몇 일 후나 전으로 계산하는 경우가 많습니다. 하지만 이러한 날짜 계산은 더 많은 상황에서 유용할 수 있습니다. 예를 들어, 약속 날짜를 정하거나 이전 이벤트의 정확한 날짜를 찾는데 사용할 수 있습니다. 이러한 이유로 날짜 계산은 프로그래밍에서 매우 중요합니다.

## 어떻게?

우리는 파이썬의 datetime 라이브러리를 사용하여 날짜 계산을 할 수 있습니다. 다음은 예시 코드와 함께 나와있습니다.

```python
# 현재 날짜를 가져오는 코드
import datetime
today = datetime.date.today()
print(today)

# 현재 날짜에서 1일 후의 날짜를 구하는 코드
import datetime
today = datetime.date.today()

# timedelta 객체를 사용하여 날짜에 값을 더해줍니다.
tomorrow = today + datetime.timedelta(days=1)

print(tomorrow)
```

위의 코드에서 우리는 현재 날짜를 가져오고, timedelta 객체를 사용하여 이후의 날짜를 구하는 방법을 배웠습니다. 또한, 이러한 방법을 사용하여 이전 날짜도 구할 수 있습니다. 여러분은 timedelta 객체에 음수 값을 전달하면 됩니다.

## 깊게 파보기

위의 예시에서는 매우 간단한 코드를 사용했지만, 날짜 계산에는 여러 가지 복잡한 방법이 있습니다. 우선, timedelta 객체에는 여러 가지 인자를 전달할 수 있으며, 일 수 뿐만 아니라 초, 분, 시간 등 다양한 단위로 값을 더하거나 빼줄 수 있습니다. 또한, datetime 라이브러리에는이 외에도 다양한 함수와 메소드가 있으므로, 여러분들은 본인이 필요한 기능에 따라 적절한 메소드를 찾아 사용할 수 있습니다.

## 이어서 보기

- [datetime 라이브러리 공식 문서](https://docs.python.org/ko/3/library/datetime.html)
- [파이썬 날짜 계산 예제](https://github.com/keon/algorithms/blob/master/time_and_space/datetime.py)
- [파이썬 날짜와 시간 처리](https://dojang.io/mod/page/view.php?id=2463)