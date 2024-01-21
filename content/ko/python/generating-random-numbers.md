---
title:                "난수 생성하기"
date:                  2024-01-20T17:49:53.983833-07:00
model:                 gpt-4-1106-preview
simple_title:         "난수 생성하기"
programming_language: "Python"
category:             "Python"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
랜덤 숫자를 생성하는 것은 예측할 수 없는 수를 만드는 것입니다. 개발자들은 게임, 시뮬레이션, 보안 알고리즘 등 다양한 상황에서 무작위성이 필요하여 이를 사용합니다.

## How to (방법)
```Python
import random

# 0과 1 사이 랜덤 실수 생성
random_float = random.random()
print(random_float)

# 1부터 10 사이 랜덤 정수 생성
random_int = random.randint(1, 10)
print(random_int)

# 리스트에서 랜덤하게 항목 선택
choices = ['사과', '배', '복숭아']
random_choice = random.choice(choices)
print(random_choice)
```

Sample Output (출력 예시):
```
0.4367912331
7
복숭아
```

## Deep Dive (심층 분석)
1970년대부터 프로그래밍에서 랜덤 숫자 생성은 중요한 역할을 해왔습니다. Python의 `random` 모듈은 난수 생성기(PRNG – Pseudo Random Number Generator)를 사용합니다. 진짜 무작위수가 아닌, 일정한 알고리즘을 바탕으로 "가짜" 랜덤 숫자를 생성하지만, 대부분의 경우엔 충분히 무작위처럼 보입니다. 대안으로, `os.urandom`이나 `secrets` 모듈을 사용하여 더 높은 보안을 요하는 애플리케이션을 위한 더 나은 무작위성을 제공할 수 있습니다. `numpy`와 같은 외부 라이브러리도 더 다양한 난수 생성 옵션을 제공합니다.

## See Also (참고자료)
- Python `random` 모듈 공식 문서: https://docs.python.org/3/library/random.html
- `numpy` 랜덤 모듈: https://numpy.org/doc/stable/reference/random/index.html
- 보안 관련 난수 생성 `secrets` 모듈 공식 문서: https://docs.python.org/3/library/secrets.html