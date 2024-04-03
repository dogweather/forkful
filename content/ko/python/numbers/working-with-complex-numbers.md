---
date: 2024-01-26 04:45:45.487380-07:00
description: "\uC0AC\uC6A9 \uBC29\uBC95: \uD30C\uC774\uC36C\uC740 \uBCF5\uC18C\uC218\
  \uC5D0 \uB300\uD55C \uB0B4\uC7A5 \uC9C0\uC6D0\uC744 \uC81C\uACF5\uD569\uB2C8\uB2E4\
  . \uC774\uB294 \uBCF5\uC18C\uC218\uB97C \uB2E4\uB8E8\uB294 \uBC29\uBC95\uC785\uB2C8\
  \uB2E4."
lastmod: '2024-03-13T22:44:54.588492-06:00'
model: gpt-4-0125-preview
summary: "\uD30C\uC774\uC36C\uC740 \uBCF5\uC18C\uC218\uC5D0 \uB300\uD55C \uB0B4\uC7A5\
  \ \uC9C0\uC6D0\uC744 \uC81C\uACF5\uD569\uB2C8\uB2E4."
title: "\uBCF5\uC18C\uC218 \uB2E4\uB8E8\uAE30"
weight: 14
---

## 사용 방법:
파이썬은 복소수에 대한 내장 지원을 제공합니다. 이는 복소수를 다루는 방법입니다:

```Python
# 복소수 생성하기
z = 4 + 5j
print(z)  # 출력: (4+5j)

# 실수부와 허수부 접근하기
print(z.real)  # 출력: 4.0
print(z.imag)  # 출력: 5.0

# 복소수 산술 연산
w = 1 - 2j
print(z + w)  # 출력: (5+3j)
print(z - w)  # 출력: (3+7j)
print(z * w)  # 출력: (14+2j)
print(z / w)  # 출력: (-3.6+1.2j)

# 모듈러스 (절대값)
print(abs(z))  # 출력: 6.4031242374328485

# 복소수의 켤레
print(z.conjugate())  # 출력: (4-5j)
```

## 심도있게 다루기
복소수는 16세기에 Gerolamo Cardano에 의해 처음 개념화되었습니다. 파이썬을 비롯한 다른 프로그래밍 언어들은 복소수를 일급 객체로 취급합니다. 이는 언어에 내장되어 있으며, 기본 연산을 위해 외부 라이브러리를 가져오는 것을 피하면서 쉽게 사용할 수 있는 기능을 의미합니다.

그러나, 깊은 수치 계산에 있어서, 파이썬에는 `cmath`라는 복소수 전용 라이브러리가 있습니다. 이는 `exp`, `log`, 삼각 함수 연산 같은 추가 기능을 가지고 있습니다.

파이썬만으로 충분하지 않을 때는, 특히 복소수를 포함하는 배열 연산에 대해 NumPy와 같은 라이브러리로 전환할 수 있습니다. NumPy는 수치 계산에서 성능에 결정적인 역할을 하는 최적화된 벡터화 연산을 제공합니다.

## 참고자료
더 알아보려면 다음 자료를 확인하세요:

- 복소수에 대한 파이썬 공식 문서: https://docs.python.org/3/library/stdtypes.html#typesnumeric
- `cmath` 모듈 문서: https://docs.python.org/3/library/cmath.html
- 복소수 배열 처리를 위한 NumPy: https://numpy.org/doc/stable/user/absolute_beginners.html#the-basics
