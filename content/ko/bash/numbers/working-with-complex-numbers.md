---
date: 2024-01-26 04:37:22.197965-07:00
description: "\uBCF5\uC18C\uC218\uB294 \uC2E4\uC218\uBD80\uC640 \uD5C8\uC218\uBD80\
  \uB85C \uAD6C\uC131\uB429\uB2C8\uB2E4. \uC2E0\uD638 \uCC98\uB9AC, \uC591\uC790\uC5ED\
  \uD559 \uB4F1\uC758 \uBD84\uC57C\uC5D0\uC11C, \uADF8\uB9AC\uACE0 \uACC4\uC0B0\uC774\
  \ \uC774\uB97C \uC694\uAD6C\uD560 \uB54C \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740\
  \ \uBCF5\uC18C\uC218\uB97C \uC0AC\uC6A9\uD569\uB2C8\uB2E4. \uC65C\uB0D0\uD558\uBA74\
  \ \uC77C\uBC18 \uC2E4\uC218\uB9CC\uC73C\uB85C\uB294 \uBD80\uC871\uD558\uAE30 \uB54C\
  \uBB38\uC785\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.470038-06:00'
model: gpt-4-0125-preview
summary: "\uBCF5\uC18C\uC218\uB294 \uC2E4\uC218\uBD80\uC640 \uD5C8\uC218\uBD80\uB85C\
  \ \uAD6C\uC131\uB429\uB2C8\uB2E4."
title: "\uBCF5\uC18C\uC218 \uB2E4\uB8E8\uAE30"
weight: 14
---

## 무엇이며 왜인가?
복소수는 실수부와 허수부로 구성됩니다. 신호 처리, 양자역학 등의 분야에서, 그리고 계산이 이를 요구할 때 프로그래머들은 복소수를 사용합니다. 왜냐하면 일반 실수만으로는 부족하기 때문입니다.

## 사용 방법:
Bash는 기본적으로 복소수를 지원하지 않습니다. 종종 `-l` 옵션을 가진 외부 도구 `bc`를 사용합니다. 다음은 bash에서 복소수를 처리하는 방법입니다:

```bash
echo "sqrt(-1)" | bc -l
```

출력:
```bash
j
```

곱셈:

```bash
echo "(-1 + -1i) * (4 + 3i)" | bc -l
```

출력:
```bash
-1.00000000000000000000-7.00000000000000000000i
```

## 깊이있게 탐구
복소수는 16세기부터 있었지만, Bash와 같은 스크립트 언어는 박스 바깥에서 복소수와 같은 수학적 계산에 최적화되어 있지 않습니다. 그래서 `bc`나 `awk`와 같은 다른 도구들이 종종 사용됩니다. 복소수로 작업하기 위한 대안 언어로는 `cmath` 모듈을 가진 Python과 MATLAB이 있으며, 이들은 모두 더 고급 수학 함수를 위해 구축되었습니다. Bash의 경우, 모두 도구 활용에 관한 것입니다 - `bc`는 허수 단위를 나타내기 위해 소문자 'i'를 사용하며 덧셈, 뺄셈, 곱셈, 나눗셈 같은 기본 연산을 지원합니다.

## 참고 자료
- `bc` 매뉴얼: https://www.gnu.org/software/bc/manual/html_mono/bc.html
- GNU Octave (MATLAB 대안): https://www.gnu.org/software/octave/
- Python `cmath` 모듈: https://docs.python.org/3/library/cmath.html
