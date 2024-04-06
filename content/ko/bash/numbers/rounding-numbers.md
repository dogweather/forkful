---
date: 2024-01-26 03:43:07.967536-07:00
description: "\uBC29\uBC95: \uC608\uC804\uC5D0\uB294 Bash \uC2A4\uD06C\uB9BD\uD2B8\
  \uC5D0\uC11C \uC218\uD559 \uB9C8\uBC95\uC744 \uBD80\uB9B4 \uC218 \uC788\uB294 `bc`\uB098\
  \ `printf`\uAC00 \uC5C6\uC5C8\uC2B5\uB2C8\uB2E4. \uC62C\uB4DC\uC2A4\uCFE8\uB7EC\uB4E4\
  \uC740 \uC678\uBD80 \uB3C4\uAD6C\uC5D0 \uC758\uC9C0\uD558\uAC70\uB098 \uAD50\uBB18\
  \uD55C \uD574\uACB0\uCC45\uC744 \uCC3E\uC544\uC57C \uD588\uC2B5\uB2C8\uB2E4. \uC774\
  \uC81C `bc`\uB97C \uC0AC\uC6A9\uD558\uBA74 \uC815\uBC00 \uC218\uD559\uC744 \uD560\
  \ \uC218 \uC788\uC2B5\uB2C8\uB2E4. \uAE30\uC5B5\uD558\uC138\uC694, `bc`\uB294 \uAE30\
  \uBCF8\uC801\uC73C\uB85C \uBC18\uC62C\uB9BC\uD558\uC9C0\u2026"
lastmod: '2024-04-05T22:51:09.762738-06:00'
model: gpt-4-0125-preview
summary: "\uC608\uC804\uC5D0\uB294 Bash \uC2A4\uD06C\uB9BD\uD2B8\uC5D0\uC11C \uC218\
  \uD559 \uB9C8\uBC95\uC744 \uBD80\uB9B4 \uC218 \uC788\uB294 `bc`\uB098 `printf`\uAC00\
  \ \uC5C6\uC5C8\uC2B5\uB2C8\uB2E4."
title: "\uC22B\uC790 \uBC18\uC62C\uB9BC\uD558\uAE30"
weight: 13
---

## 방법:
Bash에서 반올림하는 방법에 대해 알아보겠습니다:

```Bash
# 'floor'를 사용하여 내림하기
echo "scale=0; 3.49/1" | bc

# 'ceiling'을 사용하여 올림하기
echo "scale=0; 3.01/1" | bc -l

# printf를 사용하여 가장 가까운 정수로 반올림하기
printf "%.0f\n" 3.49

# bc를 사용하여 가장 가까운 정수로 반올림하는 요령
echo "(3.49+0.5)/1" | bc
```

터미널 입에서 나온 샘플 출력들:

```
3  # 내림됨 (floor)
4  # 올림됨 (ceiling)
3  # 가장 가까운 값으로 반올림됨 (printf 사용)
3  # 가장 가까운 값으로 반올림됨 (bc 사용)
```

## 심층 탐구
예전에는 Bash 스크립트에서 수학 마법을 부릴 수 있는 `bc`나 `printf`가 없었습니다. 올드스쿨러들은 외부 도구에 의지하거나 교묘한 해결책을 찾아야 했습니다. 이제 `bc`를 사용하면 정밀 수학을 할 수 있습니다. 기억하세요, `bc`는 기본적으로 반올림하지 않습니다—내림합니다. 스케일 부분은 소수점 행동을 설정합니다.

대안이 있다면? `bc`로 전환하지 않고 반올림을 하기 위해 `awk`을 사용할 수 있거나 더 많은 수학적 필요에 대해 `perl`로 씨름할 수 있습니다. 자학적인 경우에는, 예를 들어 순수 Bash로 반복적인 문자열 조작을 할 수 있지만, 왜 그럴까요?

세부 사항에 대해서는, `bc`는 단순히 반올림만 하는 것이 아니라 많은 수학적 작업을 할 수 있습니다—스케일링, 사인, 루트 계산 등등을 할 수 있습니다. `printf`는 텍스트 포매팅에 더 관련이 있지만, 반올림도 할 수 있으니 불만이 없습니다.

## 또한 보기
더 많은 정보를 원하는 이들을 위해:

- GNU `bc` 매뉴얼: https://www.gnu.org/software/bc/manual/html_mono/bc.html
- Bash `printf` 명령어: https://www.gnu.org/software/bash/manual/html_node/Bash-Builtins.html#index-printf
- 반올림 및 기타 텍스트 처리를 위한 AWK 사용자 가이드: https://www.gnu.org/software/gawk/manual/gawk.html
- 더 많은 Bash 수학, 스크립팅 및 숫자 트릭: https://mywiki.wooledge.org/BashFAQ/022
