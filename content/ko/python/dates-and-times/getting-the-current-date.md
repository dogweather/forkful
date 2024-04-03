---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:48.080246-07:00
description: "\uD30C\uC774\uC36C\uC5D0\uC11C \uD604\uC7AC \uB0A0\uC9DC\uB97C \uAC00\
  \uC838\uC624\uB294 \uAC83\uC740 \uB85C\uAE45, \uB370\uC774\uD130 \uBD84\uC11D, \uC2DC\
  \uAC04 \uAE30\uBC18 \uACB0\uC815\uC744 \uC704\uD55C \uB9CE\uC740 \uC560\uD50C\uB9AC\
  \uCF00\uC774\uC158\uC5D0\uC11C \uD544\uC218\uC801\uC778 \uC791\uC5C5\uC785\uB2C8\
  \uB2E4. \uC774\uB294 \uC2DC\uC2A4\uD15C\uC758 \uD604\uC7AC \uB0A0\uC9DC\uB97C \uAC80\
  \uC0C9\uD558\uB294 \uAC83\uACFC \uAD00\uB828\uC774 \uC788\uC73C\uBA70, \uC2DC\uAC04\
  \uC801 \uB9E5\uB77D\uC5D0 \uB530\uB77C \uB2EC\uB77C\uC9C0\uB294 \uC791\uC5C5\uC5D0\
  \uB294 \uD544\uC218\uC801\uC785\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:54.614153-06:00'
model: gpt-4-0125-preview
summary: "\uD30C\uC774\uC36C\uC5D0\uC11C \uD604\uC7AC \uB0A0\uC9DC\uB97C \uAC00\uC838\
  \uC624\uB294 \uAC83\uC740 \uB85C\uAE45, \uB370\uC774\uD130 \uBD84\uC11D, \uC2DC\uAC04\
  \ \uAE30\uBC18 \uACB0\uC815\uC744 \uC704\uD55C \uB9CE\uC740 \uC560\uD50C\uB9AC\uCF00\
  \uC774\uC158\uC5D0\uC11C \uD544\uC218\uC801\uC778 \uC791\uC5C5\uC785\uB2C8\uB2E4\
  ."
title: "\uD604\uC7AC \uB0A0\uC9DC \uAC00\uC838\uC624\uAE30"
weight: 29
---

## 무엇 & 왜?

파이썬에서 현재 날짜를 가져오는 것은 로깅, 데이터 분석, 시간 기반 결정을 위한 많은 애플리케이션에서 필수적인 작업입니다. 이는 시스템의 현재 날짜를 검색하는 것과 관련이 있으며, 시간적 맥락에 따라 달라지는 작업에는 필수적입니다.

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
