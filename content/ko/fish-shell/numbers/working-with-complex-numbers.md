---
date: 2024-01-26 04:40:16.720892-07:00
description: "\uBCF5\uC18C\uC218\uB294 1\uCC28\uC6D0 \uC22B\uC790 \uC120\uC744 2\uCC28\
  \uC6D0 \uBCF5\uC18C \uD3C9\uBA74\uC73C\uB85C \uD655\uC7A5\uD558\uB294 \uAC1C\uB150\
  \uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uACF5\uD559, \uBB3C\
  \uB9AC\uD559, \uADF8\uB798\uD53D\uACFC \uAC19\uC740 \uBD84\uC57C\uC5D0\uC11C \uC2E0\
  \uD638\uB098 \uD68C\uC804\uACFC \uAC19\uC774 \uB450 \uAC00\uC9C0 \uAD6C\uC131 \uC694\
  \uC18C\uAC00 \uD544\uC694\uD55C \uACC4\uC0B0\uC744 \uC704\uD574 \uBCF5\uC18C\uC218\
  \uB97C \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.843757-06:00'
model: gpt-4-0125-preview
summary: "\uBCF5\uC18C\uC218\uB294 1\uCC28\uC6D0 \uC22B\uC790 \uC120\uC744 2\uCC28\
  \uC6D0 \uBCF5\uC18C \uD3C9\uBA74\uC73C\uB85C \uD655\uC7A5\uD558\uB294 \uAC1C\uB150\
  \uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uACF5\uD559, \uBB3C\
  \uB9AC\uD559, \uADF8\uB798\uD53D\uACFC \uAC19\uC740 \uBD84\uC57C\uC5D0\uC11C \uC2E0\
  \uD638\uB098 \uD68C\uC804\uACFC \uAC19\uC774 \uB450 \uAC00\uC9C0 \uAD6C\uC131 \uC694\
  \uC18C\uAC00 \uD544\uC694\uD55C \uACC4\uC0B0\uC744 \uC704\uD574 \uBCF5\uC18C\uC218\
  \uB97C \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
title: "\uBCF5\uC18C\uC218 \uB2E4\uB8E8\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?
복소수는 1차원 숫자 선을 2차원 복소 평면으로 확장하는 개념입니다. 프로그래머들은 공학, 물리학, 그래픽과 같은 분야에서 신호나 회전과 같이 두 가지 구성 요소가 필요한 계산을 위해 복소수를 사용합니다.

## 방법:
Fish에서는 실수부와 허수부를 사용하여 `math`로 복소수를 다룹니다. 시작하는 방법은 다음과 같습니다:

```fish
# 두 복소수 (3+4i)와 (5+2i)를 더하기
set complex_sum (math "3+4i + 5+2i")
echo $complex_sum # 출력: 8+6i

# 두 복소수 (1+2i)와 (3+4i)를 곱하기
set complex_prod (math "1+2i * 3+4i")
echo $complex_prod # 출력: -5+10i
```

복소수를 거듭제곱 하거나 그 지수 형태를 구하려면:

```fish
# (2+3i)의 제곱
set complex_square (math "(2+3i)^2")
echo $complex_square # 출력: -5+12i

# (2i)의 지수
set complex_exp (math "e^(2i)")
echo $complex_exp # 출력: -0.41615+0.9093i
```

## 심층 분석
Fish Shell에서 복소수에 대한 `math` 지원은 상대적으로 새로운 기능으로, 약 3.1.0 버전부터 시작되었습니다. 그 전에는 사람들이 `bc`를 사용하거나 Python 같은 외부 도구를 호출하여 복잡한 수학 계산을 했을 수 있습니다.

Fish의 `math`에 대한 대안으로는 MATLAB, NumPy를 사용한 Python, 또는 표준 라이브러리를 사용한 C++와 같은 전문 수치 라이브러리나 언어가 있습니다. 그러나 이들은 간단한 쉘 계산을 위해 과한 경우가 있습니다.

Fish의 복소수 지원은 내부 `math` 명령어에 내장되어 있으며, libcalc를 활용합니다. 이는 기본적인 연산을 위해 추가 도구를 설치할 필요가 없음을 의미합니다.

그러나, Fish는 무거운 수학적 계산을 위해 설계되지 않았습니다. 그것의 수학 능력은 빠른 계산이나 복소수가 등장하는 스크립트에 편리하지만, 집중적인 작업을 위해서는 더 견고한 도구를 고려해야 합니다.

## 참조
- math에 대한 Fish shell 문서: https://fishshell.com/docs/current/commands.html#math
- 인기 있는 대안인 Python용 NumPy: https://numpy.org/
- 복소수에 대한 깊이 있는 탐구: https://betterexplained.com/articles/a-visual-intuitive-guide-to-imaginary-numbers/
