---
date: 2024-01-26 03:46:22.998726-07:00
description: "\uC22B\uC790\uB97C \uBC18\uC62C\uB9BC\uD55C\uB2E4\uB294 \uAC83\uC740\
  \ \uADF8\uAC83\uB4E4\uC744 \uB354 \uAC04\uB2E8\uD558\uAC70\uB098 \uB354 \uC911\uC694\
  \uD55C \uAC12\uC5D0 \uAC00\uAE5D\uAC8C \uC870\uC815\uD558\uB294 \uAC83\uC744 \uC758\
  \uBBF8\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uACB0\uACFC\
  \uB97C \uB2E8\uC21C\uD654\uD558\uAC70\uB098, \uD45C\uC2DC\uC6A9 \uC18C\uC218\uC810\
  \ \uC790\uB9BF\uC218\uB97C \uC81C\uD55C\uD558\uAC70\uB098, \uD2B9\uC815 \uC218\uD559\
  \uC801 \uBAA9\uC801\uC744 \uC704\uD574 \uC22B\uC790\uB97C \uBC18\uC62C\uB9BC\uD569\
  \uB2C8\uB2E4."
lastmod: '2024-03-11T00:14:28.504894-06:00'
model: gpt-4-0125-preview
summary: "\uC22B\uC790\uB97C \uBC18\uC62C\uB9BC\uD55C\uB2E4\uB294 \uAC83\uC740 \uADF8\
  \uAC83\uB4E4\uC744 \uB354 \uAC04\uB2E8\uD558\uAC70\uB098 \uB354 \uC911\uC694\uD55C\
  \ \uAC12\uC5D0 \uAC00\uAE5D\uAC8C \uC870\uC815\uD558\uB294 \uAC83\uC744 \uC758\uBBF8\
  \uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uACB0\uACFC\uB97C\
  \ \uB2E8\uC21C\uD654\uD558\uAC70\uB098, \uD45C\uC2DC\uC6A9 \uC18C\uC218\uC810 \uC790\
  \uB9BF\uC218\uB97C \uC81C\uD55C\uD558\uAC70\uB098, \uD2B9\uC815 \uC218\uD559\uC801\
  \ \uBAA9\uC801\uC744 \uC704\uD574 \uC22B\uC790\uB97C \uBC18\uC62C\uB9BC\uD569\uB2C8\
  \uB2E4."
title: "\uC22B\uC790 \uBC18\uC62C\uB9BC\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇인가 & 왜인가?
숫자를 반올림한다는 것은 그것들을 더 간단하거나 더 중요한 값에 가깝게 조정하는 것을 의미합니다. 프로그래머들은 결과를 단순화하거나, 표시용 소수점 자릿수를 제한하거나, 특정 수학적 목적을 위해 숫자를 반올림합니다.

## 방법:
Python에서 숫자를 반올림하는 방법에 대한 정보입니다:

```python
# 가장 가까운 정수로 반올림
print(round(8.67))  # 출력: 9

# 지정된 소수점 자릿수로 반올림
print(round(8.67, 1))  # 출력: 8.7

# 공동 자리수의 경우 짝수는 내림, 홀수는 올림
print(round(2.5))  # 출력: 2
print(round(3.5))  # 출력: 4
```

## 심층 분석
Python에서 `round()`는 단순히 소수점을 잘라내는 것이 아닙니다. 역사적으로 Python은 다른 많은 언어들처럼 "반올림 반짝이는 짝수" 또는 "은행가 반올림"을 따릅니다. 이는 금융 계산에서 중요한 합계나 평균의 누적 오류를 최소화합니다.

대안으로, Python의 수학 모듈에서 제공하는 `math.floor()`와 `math.ceil()`이 있어 숫자를 다음 정수로 내리거나 올립니다. 그러나 정밀도가 중요하다면, `decimal` 모듈의 `quantize()`를 사용하여 반올림 행동을 지정할 수 있습니다.

내부적으로, `round()`는 이진 부동소수점 숫자를 다룹니다. 일부 소수는 이진수로 정확히 표현될 수 없기 때문에, `round(2.675, 2)`가 예상대로 `2.68`이 되지 않는 것과 같은 놀라움을 겪을 수 있습니다. 이 경우 `decimal`이나 `fractions`을 사용해 고정밀도를 얻을 수 있습니다.

## 참고 자료
- Python의 내장 함수에 대한 문서: https://docs.python.org/3/library/functions.html#round
- Decimal 고정 소수점 및 부동 소수점 산술: https://docs.python.org/3/library/decimal.html
- Python의 수학 모듈: https://docs.python.org/3/library/math.html
