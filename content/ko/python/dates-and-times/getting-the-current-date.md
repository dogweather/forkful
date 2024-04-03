---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:48.080246-07:00
description: "\uBC29\uBC95: **\uD45C\uC900 \uB77C\uC774\uBE0C\uB7EC\uB9AC `datetime`\
  \ \uC0AC\uC6A9\uD558\uAE30:** \uD30C\uC774\uC36C\uC758 \uD45C\uC900 \uB77C\uC774\
  \uBE0C\uB7EC\uB9AC\uC5D0 \uD3EC\uD568\uB41C `datetime` \uBAA8\uB4C8\uC740 \uB0A0\
  \uC9DC\uC640 \uC2DC\uAC04\uC744 \uC870\uC791\uD558\uAE30 \uC704\uD55C \uD074\uB798\
  \uC2A4\uB4E4\uC744 \uC81C\uACF5\uD569\uB2C8\uB2E4. \uD604\uC7AC \uB0A0\uC9DC\uB97C\
  \ \uC5BB\uAE30 \uC704\uD574\uC11C\uB294 `date.today()` \uBA54\uC18C\uB4DC\uB97C\
  \ \uC0AC\uC6A9\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:54.614153-06:00'
model: gpt-4-0125-preview
summary: "**\uD45C\uC900 \uB77C\uC774\uBE0C\uB7EC\uB9AC `datetime` \uC0AC\uC6A9\uD558\
  \uAE30:**\n\n\uD30C\uC774\uC36C\uC758 \uD45C\uC900 \uB77C\uC774\uBE0C\uB7EC\uB9AC\
  \uC5D0 \uD3EC\uD568\uB41C `datetime` \uBAA8\uB4C8\uC740 \uB0A0\uC9DC\uC640 \uC2DC\
  \uAC04\uC744 \uC870\uC791\uD558\uAE30 \uC704\uD55C \uD074\uB798\uC2A4\uB4E4\uC744\
  \ \uC81C\uACF5\uD569\uB2C8\uB2E4."
title: "\uD604\uC7AC \uB0A0\uC9DC \uAC00\uC838\uC624\uAE30"
weight: 29
---

## 방법:
**표준 라이브러리 `datetime` 사용하기:**

파이썬의 표준 라이브러리에 포함된 `datetime` 모듈은 날짜와 시간을 조작하기 위한 클래스들을 제공합니다. 현재 날짜를 얻기 위해서는 `date.today()` 메소드를 사용할 수 있습니다.

```python
from datetime import date

today = date.today()
print(today)  # 출력: YYYY-MM-DD (예: 2023-04-05)
```

**시간 형식 지정:**

다른 형식의 현재 날짜가 필요한 경우, `strftime` 메소드를 사용하면 원하는 날짜 형식을 지정할 수 있습니다:

```python
from datetime import date

today = date.today()
formatted_date = today.strftime('%B %d, %Y')  # 예시 형식: "April 05, 2023"
print(formatted_date)
```

**더 많은 유연성을 위해 `pendulum` 사용하기 (인기 있는 타사 라이브러리):**

`Pendulum`은 파이썬에서 날짜와 시간을 다루는 좀 더 직관적인 접근 방법을 제공하는 타사 라이브러리입니다. 이는 표준 datetime 기능을 확장하고, 시간대 관리를 간소화하는 등의 기능을 포함하고 있습니다.

먼저, pip를 통해 `pendulum`을 설치했는지 확인하세요:

```shell
pip install pendulum
```

그런 다음, 현재 날짜를 얻으려면:

```python
import pendulum

today = pendulum.now().date()
print(today)  # 출력: YYYY-MM-DD (예: 2023-04-05)
```

`Pendulum`을 사용하면, 형식 지정도 `strftime` 접근 방식과 유사하고 간단합니다:

```python
import pendulum

today = pendulum.now()
formatted_date = today.to_formatted_date_string()  # 기본 형식: "Apr 5, 2023"
print(formatted_date)
```
