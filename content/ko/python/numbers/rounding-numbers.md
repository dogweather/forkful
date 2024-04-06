---
date: 2024-01-26 03:46:22.998726-07:00
description: "\uBC29\uBC95: Python\uC5D0\uC11C `round()`\uB294 \uB2E8\uC21C\uD788\
  \ \uC18C\uC218\uC810\uC744 \uC798\uB77C\uB0B4\uB294 \uAC83\uC774 \uC544\uB2D9\uB2C8\
  \uB2E4. \uC5ED\uC0AC\uC801\uC73C\uB85C Python\uC740 \uB2E4\uB978 \uB9CE\uC740 \uC5B8\
  \uC5B4\uB4E4\uCC98\uB7FC \"\uBC18\uC62C\uB9BC \uBC18\uC9DD\uC774\uB294 \uC9DD\uC218\
  \" \uB610\uB294 \"\uC740\uD589\uAC00 \uBC18\uC62C\uB9BC\"\uC744 \uB530\uB985\uB2C8\
  \uB2E4. \uC774\uB294 \uAE08\uC735 \uACC4\uC0B0\uC5D0\uC11C \uC911\uC694\uD55C \uD569\
  \uACC4\uB098 \uD3C9\uADE0\uC758 \uB204\uC801 \uC624\uB958\uB97C \uCD5C\uC18C\uD654\
  \uD569\uB2C8\uB2E4. \uB300\uC548\uC73C\uB85C,\u2026"
lastmod: '2024-04-05T22:51:09.081311-06:00'
model: gpt-4-0125-preview
summary: "Python\uC5D0\uC11C `round()`\uB294 \uB2E8\uC21C\uD788 \uC18C\uC218\uC810\
  \uC744 \uC798\uB77C\uB0B4\uB294 \uAC83\uC774 \uC544\uB2D9\uB2C8\uB2E4."
title: "\uC22B\uC790 \uBC18\uC62C\uB9BC\uD558\uAE30"
weight: 13
---

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
