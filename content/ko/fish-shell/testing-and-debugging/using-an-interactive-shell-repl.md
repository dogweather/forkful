---
date: 2024-01-26 04:14:26.449869-07:00
description: "\uC0AC\uC6A9 \uBC29\uBC95: Fish\uC5D0\uC11C\uB294 \uC2DC\uC791\uD560\
  \ \uB54C \uC0C1\uD638\uC791\uC6A9\uD615 \uC178\uC774 \uAE30\uBCF8 \uBAA8\uB4DC\uC785\
  \uB2C8\uB2E4. \uB2E4\uC74C\uC740 \uC791\uB3D9\uD558\uB294 \uBAA8\uC2B5\uC785\uB2C8\
  \uB2E4."
lastmod: '2024-03-13T22:44:55.857899-06:00'
model: gpt-4-0125-preview
summary: "Fish\uC5D0\uC11C\uB294 \uC2DC\uC791\uD560 \uB54C \uC0C1\uD638\uC791\uC6A9\
  \uD615 \uC178\uC774 \uAE30\uBCF8 \uBAA8\uB4DC\uC785\uB2C8\uB2E4."
title: "\uC778\uD130\uB799\uD2F0\uBE0C \uC178 (REPL) \uC0AC\uC6A9\uD558\uAE30"
weight: 34
---

## 사용 방법:
Fish에서는 시작할 때 상호작용형 셸이 기본 모드입니다. 다음은 작동하는 모습입니다:

```Fish Shell
> set color blue
> echo "The sky is $color"
하늘은 파란색입니다
```

내장 함수를 실행하고 명령 치환을 사용해보는 것도 가능합니다:

```Fish Shell
> function cheer
      echo "고 Fish $argv!"
  end
> cheer 개발자들
고 Fish 개발자들!
```

함수를 정의하는 것뿐만 아니라, 코드 조각을 즉석에서 실행하고 즉시 출력을 볼 수도 있습니다:

```Fish Shell
> math "40 / 2"
20
```

## 심층 분석
REPL의 개념은 1960년대 Lisp 프로그래밍 언어로 거슬러 올라갑니다. 이러한 상호작용형 프로그래밍 형태는 Python의 `ipython`이나 Ruby의 `irb` 같은 환경에 대한 벤치마크를 설정했습니다. Fish는 사용자 친화성과 상호작용적 사용에 중점을 두면서 이러한 추세를 이어갑니다.

다른 셸들인 Bash와는 달리 Fish는 처음부터 상호작용성을 염두에 두고 설계되었습니다. 구문 강조, 자동 제안, 탭 자동 완성 등을 제공하여 REPL 스타일 워크플로우에서 강력하게 사용할 수 있습니다. 더 좋은 점은, 명령이 기억되고 검색 가능하며, 반복 테스트를 수월하게 만듭니다.

Fish의 REPL에 대한 대안으로는 `bash-completion`이나 `oh-my-zsh` 같은 확장 기능과 함께 사용할 때의 `bash`나 `zsh`가 있을 수 있지만, Fish는 개시부터 더 풍부한 경험을 제공하는 경향이 있습니다.

## 또한 보십시오:
- Fish 문서: https://fishshell.com/docs/current/index.html
- Fish와 다른 셸의 흥미로운 비교: https://www.slant.co/versus/2209/3686/~fish_vs_bash
- REPL에 대한 더 깊은 탐구: https://en.wikipedia.org/wiki/Read–eval–print_loop
- Lisp의 상호작용형 프로그래밍, 역사적인 모습: http://www.paulgraham.com/ilisp.html
