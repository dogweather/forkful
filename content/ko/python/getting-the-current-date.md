---
title:                "현재 날짜 가져오기 (yeonjaye naljja gajyeogi)"
html_title:           "Python: 현재 날짜 가져오기 (yeonjaye naljja gajyeogi)"
simple_title:         "현재 날짜 가져오기 (yeonjaye naljja gajyeogi)"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 왜

현재 날짜를 얻는 것이 왜 중요한지 궁금해하고 있는 사람들에게 이 문서를 소개합니다. 파이썬에서 현재 날짜를 가져오는 방법을 간략하게 배워보겠습니다.

## 어떻게

파이썬에서 현재 날짜를 가져오는 가장 간단한 방법은 `datetime` 모듈을 사용하는 것입니다. 우선 `datetime` 모듈을 임포트해야 합니다.

```Python
import datetime
```

이제 `datetime` 모듈의 `datetime` 클래스를 사용하여 현재 날짜와 시간을 생성할 수 있습니다.

```Python
now = datetime.datetime.now()
print(now) # 2021-08-12 14:57:24.393608
```

위의 예제에서는 `now()` 메서드를 사용해 현재 날짜와 시간을 출력하는 것을 볼 수 있습니다. 하지만 `now()` 메서드는 개인 컴퓨터의 시스템 시간을 기준으로 날짜와 시간을 가져오므로, 실행할 때마다 값이 달라질 수 있습니다.

더 정확한 현재 시간을 얻고 싶다면 `utcnow()` 메서드를 사용하는 것이 좋습니다. `utcnow()` 메서드는 협정 세계시(UTC) 기준으로 날짜와 시간을 가져오기 때문에 값의 변화가 없습니다.

```Python
now = datetime.datetime.utcnow()
print(now) # 2021-08-12 05:57:24.393608
```

하지만 위의 예제에서는 시간의 차이를 고려하지 않고 UTC 기준으로만 값을 출력하기 때문에 우리나라 시간대를 맞추고 싶다면 `now()` 메서드와 같이 사용하면 됩니다.

```Python
now = datetime.datetime.utcnow()
kor_now = now + datetime.timedelta(hours=9)
print(kor_now) # 2021-08-12 14:57:24.393608
```

위의 예제에서는 `timedelta`를 사용하여 시간의 차이를 9시간으로 설정한 후 `now()` 메서드와 더해주는 방식으로 한국 시간대를 맞춘 것을 볼 수 있습니다.

## 딥 다이브

파이썬에서 현재 날짜를 가져오는 방법은 `datetime` 모듈 뿐만 아니라 `time` 모듈이나 `calendar` 모듈에도 다양한 메서드를 제공합니다. 또한 날짜와 시간을 조작하고 형식을 바꾸는 방법도 다양하기 때문에 참고해보시기 바랍니다.

## 참고할만한 다른 내용

- 파이썬 공식 문서: https://docs.python.org/ko/3/library/datetime.html
- 한국어 블로그 "파이썬 날짜와 시간 다루기": https://koreanfoodie2.tistory.com/106